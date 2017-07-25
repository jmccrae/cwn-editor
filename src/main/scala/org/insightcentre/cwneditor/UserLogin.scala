package org.insightcentre.cwneditor

import java.io.File
import java.math.BigInteger
import java.security.NoSuchAlgorithmException
import java.security.SecureRandom
import java.security.spec.InvalidKeySpecException
import java.sql.DriverManager
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import scala.math.min
import sql._


trait UserLogin {
  def login(username : String, password : String) : Option[String]
  def updateUser(username : String, oldPassword : String, newPassword : String) : Boolean
  def addUser(username : String, password : String, email : String) : Boolean
}

object SQLLogin extends UserLogin {
  try {
    Class.forName("org.sqlite.JDBC") }
  catch {
    case x : ClassNotFoundException => throw new RuntimeException("No Database Driver", x) }

  private val file = new File("users.db")
  def conn = {
    DriverManager.getConnection("jdbc:sqlite:" + file.getPath())
  }

  if(!file.exists) {
    withSession(conn) { implicit session =>
      sql"""CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT,
                                name TEXT UNIQUE,
                                hash TEXT,
                                email TEXT)""".execute
    }
  }

  private val PBKDF2_ITERATIONS = 1000
  private val HASH_BYTES = 24
  private val PBKDF2_ALGORITHM = "PBKDF2WithHmacSHA1"

  private def mkSalt = {
    val random = new SecureRandom()
    val salt = new Array[Byte](HASH_BYTES)
    random.nextBytes(salt)

    salt
  }


  private def hash(password : String) = {
    val salt = mkSalt

    val hash = pbkdf2(password.toCharArray, salt, PBKDF2_ITERATIONS, HASH_BYTES)

    toHex(salt) + ":" +  toHex(hash)
  }

  private def toBytes(hex: String): Array[Byte] = {
    hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

   private def toHex(bytes: Array[Byte]): String = {
     bytes.map("%02x".format(_)).mkString
   }

  private def validate(password : String, hashString : String) = {
    if(hashString == "") {
      true
    } else {
      val (salt, hash) = hashString.split(":") match {
        case Array(s, h) => (toBytes(s), toBytes(h))
      }
      val testHash = pbkdf2(password.toCharArray, salt, PBKDF2_ITERATIONS, hash.length)
      slowEquals(hash, testHash)
    }
  }

  private def slowEquals(a : Array[Byte], b : Array[Byte]) = {
    var diff = a.length ^ b.length;
    for(i <- 0 until min(a.length, b.length)) {
      diff |= a(i) ^ b(i)
    }
    diff == 0
  }

  private def pbkdf2(password : Array[Char], salt : Array[Byte], 
    iterations : Int, bytes : Int) : Array[Byte] = {
    val spec = new PBEKeySpec(password, salt, iterations, bytes * 8)
    val skf = SecretKeyFactory.getInstance(PBKDF2_ALGORITHM)
    skf.generateSecret(spec).getEncoded()
  }

  def login(username : String, password : String) = withSession(conn) { implicit session =>
    sql"""SELECT hash FROM users WHERE name=${username}""".as1[String].headOption match {
      case Some(hash) => if(validate(password, hash)) {
        Some(toHex(mkSalt))
      } else {
        None
      }
      case None => {
        None
      }
    }
  }

  def updateUser(username : String, oldPassword : String, newPassword : String) = 
    withSession(conn) { implicit session =>
      if(login(username, oldPassword) != None) {
        val h = hash(newPassword)
        sql"""UPDATE users SET hash=${h} WHERE name=${username}""".execute
        true
      } else {
        false
      }
    }

  def addUser(username : String, password : String, email : String) =
    withSession(conn) { implicit session =>
      val h = hash(password)
      sql"""INSERT INTO users (name, hash, email) VALUES (${username}, ${h}, ${email})""".execute
      true
    }
}

import java.time.Instant
import java.time.temporal.ChronoUnit._

class TimedHash {
  private val session = (24, HOURS)
  private val underlying = collection.mutable.HashMap[String, (String, Instant)]()

  def get(key : String) : Option[String] = {
    clean()
    underlying.get(key).map(_._1)
  }

  def put(key : String, value : String) = {
    clean()
    underlying.put(key, (value, Instant.now()))
  }

  private def clean() {
    val elems = underlying.filter(x => x._2._2.until(Instant.now(), session._2) > session._1).keys
    for(k <- elems) {
      underlying.remove(k)
    }
  }

  def clear(key : String) {
    clean()
    underlying.remove(key)
  }
}
