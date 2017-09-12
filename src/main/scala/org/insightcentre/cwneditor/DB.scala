package org.insightcentre.cwneditor.db

import java.io.File
import java.security.NoSuchAlgorithmException
import java.security.SecureRandom
import java.security.spec.InvalidKeySpecException
import java.sql.Connection
import java.sql.DriverManager
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import org.insightcentre.cwneditor._
import org.insightcentre.cwneditor.sql._
import scala.collection.JavaConversions._
import scala.math.min
import spray.json._

case class WordNetEntry(word : String, ili : String, definition : String)

case class AnnotationQueueEntry(id : Int, expiry : Long, lemma : String, user : String, examples : List[String]) {
  def expiryString = AnnotationQueueEntry.dateFormat.format(new java.util.Date(expiry))

  def toEntry : Entry = Entry(lemma, "", examples.map({e => Example(e)}),
    "", Nil)
}

object AnnotationQueueEntry {
  val dateFormat = new java.text.SimpleDateFormat("E d MMMM YYYY")
}


class DB(db : File) {
  import CWNEditorJsonProtocol._
  val HOLD_LENGTH_DAYS = 7

  try {
    Class.forName("org.sqlite.JDBC") }
  catch {
    case x : ClassNotFoundException => throw new RuntimeException("No Database Driver", x) }

 
  def init : DBLoader = {
    val c = DriverManager.getConnection("jdbc:sqlite:" + db.getPath())
    new DBLoader(c)
  }

  class DBLoader(conn : Connection) {
    implicit val session = new Session(conn)
    conn.setAutoCommit(false)

    sql"""CREATE TABLE entries (id INTEGER PRIMARY KEY AUTOINCREMENT,
      lemma TEXT UNIQUE,
      content TEXT,
      annotator TEXT,
      pwn BOOLEAN)""".execute
    sql"""CREATE INDEX entries_id ON entries (id)""".execute
    sql"""CREATE INDEX entries_lemma ON entries (lemma)""".execute
    sql"""CREATE INDEX entries_annotator ON entries (annotator)""".execute
    sql"""CREATE TABLE synsets (id INTEGER PRIMARY KEY AUTOINCREMENT,
      ili TEXT,
      definition TEXT,
      content TEXT,
      pos TEXT,
      pwn BOOLEAN)""".execute
    sql"""CREATE INDEX synsets_ili ON synsets (ili)""".execute
    sql"""CREATE TABLE entry_synset (
      entry INTEGER, synset INTEGER,
      FOREIGN KEY(entry) REFERENCES entries(id),
      FOREIGN KEY(synset) REFERENCES synsets(id))""".execute
    sql"""CREATE INDEX entry_synset_entry ON entry_synset (entry)""".execute
    sql"""CREATE INDEX entry_synset_synset ON entry_synset (synset)""".execute
    sql"""CREATE TABLE queue (id INTEGER PRIMARY KEY AUTOINCREMENT,
      expiry INTEGER,
      lemma TEXT,
      user TEXT,
      examples TEXT)""".execute
    sql"""CREATE INDEX queue_users ON queue (user)""".execute
    sql"""CREATE INDEX queue_expiry ON queue (expiry)""".execute
    sql"""CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE,
      hash TEXT,
      email TEXT,
      reviewer TEXT)""".execute


    var entries = 0
    var synsets = 0
    val insertEntryQuery = sql"""INSERT INTO entries VALUES (?, ?, ?, ?, ?)""".insert5[Int, String, String, String, Boolean]
    val insertEntrySynsetQuery = sql"""INSERT INTO entry_synset VALUES (?, ?)""".insert2[Int, Int]
    val insertSynsetQuery = sql"""INSERT INTO synsets VALUES (?, ?, ?, ?, ?, ?)""".insert6[Int, String, String, String, String, Int]
    val insertQueueEntry = sql"""INSERT INTO queue VALUES (?,?,?,?,?)""".insert5[Int,Int,String,String,String]
    val insertUserQuery = sql"""INSERT INTO users (name, hash, email, reviewer) VALUES (?, ?, ?, ?)""".insert4[String, String, String, String]
    private var qi = 0;
    
    val ILI = "([icp]\\d+)".r
    val SYN = "(.*): (.*) <(.*)>".r

    // This does not check the uniqueness constraint!
    def insertEntry(e : Entry, user : String) : Int = {
      entries += 1
      insertEntryQuery(entries, e.lemma, e.toJson.toString, user,
        !e.senses.exists(_.synset.startsWith("c")))
      if(entries % 10000 == 0) {
        insertEntryQuery.execute
      }
      entries
    }

    def insertSynset(s : Synset, user : String) : Int = {
      synsets += 1 
      insertSynsetQuery(synsets, s.id, s.definition, s.toJson.toString, 
        s.pos, if(s.id.startsWith("c")) { 0 } else { 1 })
      if(synsets % 10000 == 0) {
        insertSynsetQuery.execute
      }
      synsets
    }

    def insertEntrySynset(e : Int, s : Int) {
      insertEntrySynsetQuery(e, s)
    }

    def insertQueue(lemma : String, examples : String) {
      qi += 1
      insertQueueEntry(qi, 0, lemma, "", examples)
      if(qi % 10000 == 0) {
        insertQueueEntry.execute
      }
    }

    def insertUser(name : String, hash : String, email : String, reviewer : String) {
      insertUserQuery(name, hash, email, reviewer)
      insertUserQuery.execute
    }

    def close = {
      insertEntryQuery.execute
      insertSynsetQuery.execute
      insertEntrySynsetQuery.execute
      insertQueueEntry.execute
      conn.commit
      session.close
    }
  }

  def conn = {
    DriverManager.getConnection("jdbc:sqlite:" + db.getPath())
  }

  def list : List[String] = withSession(conn) { implicit session =>
    sql"""SELECT id FROM entries WHERE pwn=0""".as1[String].toList
  }

  def listRange(offset : Int, length : Int, recency : Boolean = false,
    annotator : Option[String] = None) : List[String] = withSession(conn) { implicit session =>
      if(recency) {
        annotator match {
          case Some(ann) =>
            sql"""SELECT id FROM entries WHERE pwn=0 AND annotator=${ann} 
            ORDER BY id DESC LIMIT ${length} OFFSET ${offset}""".as1[String].toList
          case None =>
            sql"""SELECT id FROM entries WHERE pwn=0 
            ORDER BY id DESC LIMIT ${length} OFFSET ${offset}""".as1[String].toList
        }
      } else {
        annotator match {
          case Some(ann) =>
            sql"""SELECT id FROM entries WHERE pwn=0 AND annotator=${ann}
            ORDER BY lemma LIMIT ${length} OFFSET ${offset}""".as1[String].toList
          case None =>
            sql"""SELECT id FROM entries WHERE pwn=0 
            ORDER BY lemma LIMIT ${length} OFFSET ${offset}""".as1[String].toList
        }
      }
  }

  def get(id : String) : Option[Entry] = withSession(conn) { implicit session =>
    sql"""SELECT content FROM entries WHERE id=${id} AND pwn=0""".as1[String].headOption.map(_.parseJson.convertTo[Entry])
  }

  def getEntry(id : String) : Option[Entry] = withSession(conn) { implicit session =>
    sql"""SELECT content FROM entries WHERE lemma=${id}""".as1[String].headOption.map(_.parseJson.convertTo[Entry])
  }


  def getSynset(id : String) : Option[Synset] = withSession(conn) { implicit session =>
    sql"""SELECT content FROM synsets WHERE ili=${id}""".as1[String].headOption.
    map(_.parseJson.convertTo[Synset])
  }

  def getSynsetEntries(id : String) : Seq[Entry] = withSession(conn) { implicit session =>
    sql"""SELECT entries.content FROM entries 
          JOIN entry_synset ON entry_synset.entry = entries.id
          JOIN synsets ON entry_synset.synset = synsets.id
          WHERE synsets.ili=$id""".as1[String].map(_.parseJson.convertTo[Entry]).toList
  }

  def getSynsetWithMembers(id : String) : Option[SynsetWithMembers] = withSession(conn) { implicit session =>
    sql"""SELECT entries.lemma, synsets.content FROM entries 
          JOIN entry_synset ON entry_synset.entry = entries.id
          JOIN synsets ON entry_synset.synset = synsets.id
          WHERE synsets.ili=${id}""".as2[String,String].foldLeft(None : Option[SynsetWithMembers]) {
      case (Some(e),(lemma,_)) => Some(e.copy(lemmas=e.lemmas :+ lemma))
      case (None, (lemma,js)) => {
        val synset = js.parseJson.convertTo[Synset]
        Some(SynsetWithMembers(synset.id, synset.pos, synset.definition, synset.relations,
          List(lemma)))
      }
    }
  }


  def next(id : String) : Option[String] = withSession(conn) { implicit session =>
    sql"""SELECT id FROM entries WHERE lemma=${id}""".as1[Int].headOption.flatMap({ num =>
      sql"""SELECT lemma FROM entries WHERE id>=${num + 1} AND pwn=0 LIMIT 1""".as1[String].headOption
    })
  }
  def search(pattern : String) : List[(String, Entry)] = withSession(conn) { implicit session =>
    sql"""SELECT id, content FROM entries WHERE word LIKE ${pattern.replaceAll("\\*", "%")} AND pwn=0""".
      as2[String, String].map({ 
        case (id, content) => (id, content.parseJson.convertTo[Entry])
      }).toList
  }

  private def definitions(entry : Entry) : String = {
    throw new RuntimeException("TODO")
    //entry.senses.map({sense => sense.definition}).mkString(";;;")
  }

  def update(username : String, id : String, entry : Entry) : Unit = withSession(conn) { implicit session =>
    sql"""UPDATE entries SET content=${entry.toJson.toString},
                             lemma=${entry.lemma},
                             annotator=${username}
                             WHERE lemma=${id}""".execute
  }   

  def updateSynset(id : String, synset : Synset) : String = withSession(conn) { implicit sesion =>
    if(synset.id.matches("n\\d+")) {
      val _synset  = synset.copy(id = getNextCSynset)
      sql"""INSERT INTO synsets (ili, definition, content, pos, pwn) VALUES (
        ${_synset.id}, ${_synset.definition}, ${_synset.toJson.toString}, ${_synset.pos}, 0)""".execute
      _synset.id
    } else {
      sql"""UPDATE synsets SET definition=${synset.definition},
                               content=${synset.toJson.toString},
                               pos=${synset.pos},
                               ili=${synset.id}
                           WHERE ili=${id}""".execute
      synset.id
    }
  }

  def updateEntrySynset(eid : String, entry : Entry, 
    synsets : Seq[Synset]) = withSession(conn) { implicit session =>
      sql"""DELETE FROM entry_synset WHERE entry in (
        SELECT id FROM entries WHERE lemma=${eid})""".execute
      for(synset <- synsets){
        sql"""INSERT INTO entry_synset SELECT entries.id, synsets.id FROM
        entries JOIN synsets WHERE lemma=${entry.lemma} AND ili=${synset.id}""".execute
      }

  }

  def addEntry(lemma : String, examples : List[String]) : Unit = withSession(conn) { implicit session =>
    sql"""INSERT INTO entries (lemma, content, annotator, pwn) VALUES 
      (${lemma}, ${Entry(lemma, "", examples.map(Example), "", Nil).toJson.toString},
        "", 0)""".execute
  }

  private var maxCSynset : Option[Int] = None

  private def getNextCSynset : String = this.synchronized {
    val c = maxCSynset match {
      case Some(i) =>
        i
      case None =>
        withSession(conn) { implicit session =>
          sql"""SELECT ili FROM synsets WHERE ili LIKE 'c%'""".as1[String].map(id => id.drop(1).toInt).max
        }
    }
    maxCSynset = Some(c + 1)
    s"c${c+1}"
  }

  def find(s : String) : List[WordNetEntry] = withSession(conn) { implicit session =>
    sql"""SELECT synsets.ili, entries.lemma, synsets.definition FROM entries 
          JOIN entry_synset ON entries.id == entry_synset.entry
          JOIN synsets ON synsets.id == entry_synset.synset
          WHERE definition != "" AND lemma LIKE ${s + "%"} 
          ORDER BY length(lemma) LIMIT 50""".as3[String, String, String].flatMap({
            case (id, word, defn) => defn.split(";;;").map({ defn =>
              WordNetEntry(word, id, defn)
            })
          }).toList
  }

  def get(id : Int) = withSession(conn) { implicit session =>
    sql"""SELECT * FROM queue WHERE id=$id""".as5[Int,Long,String,String,String].map({
      case (id, expiry, lemma, user, examples) => AnnotationQueueEntry(id, expiry, lemma, user, examples.split(";;;").toList)
    }).headOption
  }


  def getQueue(user : String) = withSession(conn) { implicit session =>
    val now = (java.time.LocalDate.now().toEpochDay()) * 86400000l 
    sql"""SELECT * FROM queue WHERE user=$user AND expiry > $now""".as5[Int,Long,String,String,String].map({
      case (id, expiry, lemma, user, examples) => AnnotationQueueEntry(id, expiry, lemma, user, examples.split(";;;").toList)
    }).toList
  }

  def pullQueue(user : String, n : Int) {
    withSession(conn) { implicit session =>
      val now = (java.time.LocalDate.now().toEpochDay()) * 86400000l 
      val newExpiry = (java.time.LocalDate.now().toEpochDay() + HOLD_LENGTH_DAYS) * 86400000l
      sql"""SELECT id FROM queue WHERE user="" OR expiry < $now LIMIT $n""".
        as1[Int].foreach(id => {
          sql"""UPDATE queue SET user=$user, expiry=$newExpiry WHERE id=$id""".execute
        })
    }
  }

  def dequeue(user : String, id : Int) = withSession(conn) { implicit session =>
    sql"""UPDATE queue SET user="" WHERE user=$user AND id=$id""".execute
  }

  def extend(user : String, id : Int) = withSession(conn) { implicit session =>
    val newExpiry = (java.time.LocalDate.now().toEpochDay() + HOLD_LENGTH_DAYS) * 86400000l
    sql"""UPDATE queue SET expiry=$newExpiry WHERE user=$user AND id=$id""".execute
  }

  def remove(user : String, id : Int) = withSession(conn) { implicit session =>
    sql"""DELETE FROM queue WHERE user=$user AND id=$id""".execute
  }

  def removeOrReview(user : String, id : Int) = withSession(conn) { implicit session =>
    val reviewer = sql"""SELECT reviewer FROM users WHERE name=$user""".as1[String].headOption.flatMap({
      case "" => None
      case user => Some(user)
    })
    reviewer match {
      case Some(r) if r != user =>
        sql"""UPDATE queue SET user=$r WHERE user=$user AND id=$id""".execute
      case _ =>
        sql"""DELETE FROM queue WHERE user=$user AND id=$id""".execute
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

  def addUser(username : String, password : String, email : String, reviewer : String) =
    withSession(conn) { implicit session =>
      val h = hash(password)
      try {
        sql"""INSERT INTO users (name, hash, email, reviewer) VALUES (${username}, ${h}, ${email}, ${reviewer})""".execute
        true
      } catch {
        case x : java.sql.SQLException => false
      }
    }
}
