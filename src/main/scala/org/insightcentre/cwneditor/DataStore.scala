package org.insightcentre.cwneditor

import java.io.File
import spray.json._
import scala.collection.JavaConversions._
import sql._
import java.sql.DriverManager

trait DataStore {
  //def list : List[String]
  def listRange(offset : Int, length : Int, recency : Boolean = false,
    annotator : Option[String] = None) : List[String]
  def get(id : String) : Option[Entry]
  def next(id : String) : Option[String]
  def search(pattern : String) : List[(String, Entry)]
  def update(id : String, entry : Entry) : Unit
  /** Returns the new id */
  def insert(entry : Entry, user : String) : String
  def find(s : String) : List[WordNetEntry]
}

case class WordNetEntry(word : String, ili : String, definition : String)

class SQLDataStore(db : File) extends DataStore {
  import CWNEditorJsonProtocol._
  try {
    Class.forName("org.sqlite.JDBC") }
  catch {
    case x : ClassNotFoundException => throw new RuntimeException("No Database Driver", x) }


  if(!db.exists) {
    val c = DriverManager.getConnection("jdbc:sqlite:" + db.getPath())
    withSession(c) { implicit session =>
      c.setAutoCommit(false)
      sql"""CREATE TABLE entries (num INTEGER PRIMARY KEY AUTOINCREMENT,
                                  id TEXT,
                                  word TEXT,
                                  definition TEXT,
                                  content TEXT,
                                  annotator TEXT,
                                  pwn BOOLEAN)""".execute
      sql"""CREATE INDEX entries_id ON entries (id)""".execute
      sql"""CREATE INDEX entries_word ON entries (word)""".execute
      val insertWord = sql""" INSERT INTO entries VALUES (?,?,?,?,?,?,?)""".insert7[Int,String,String,String,String,String,Boolean]
      var i = 0
      System.err.println("Loading database")
      io.Source.fromInputStream(
        new java.util.zip.GZIPInputStream(
          new java.io.FileInputStream("data.csv.gz"))).getLines.foreach({ line =>
            val e = line.split("\\|\\|\\|")
            // All previous data by jmccrae, this will be updated
            insertWord(e(0).toInt, e(1), e(2), e(3), e(4), "jmccrae", e(5) == "1")
            i += 1
            if(i % 10000 == 0) {
              insertWord.execute
              System.err.print(".")
            }
      })
      insertWord.execute
      System.err.println("")

      c.commit()
      c.setAutoCommit(true)
    }
  } //else {
    //System.err.println("Database already exists")
//  }

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
            ORDER BY num DESC LIMIT ${length} OFFSET ${offset}""".as1[String].toList
          case None =>
            sql"""SELECT id FROM entries WHERE pwn=0 
            ORDER BY num DESC LIMIT ${length} OFFSET ${offset}""".as1[String].toList
        }
      } else {
        annotator match {
          case Some(ann) =>
            sql"""SELECT id FROM entries WHERE pwn=0 AND annotator=${ann}
            LIMIT ${length} OFFSET ${offset}""".as1[String].toList
          case None =>
            sql"""SELECT id FROM entries WHERE pwn=0 
            LIMIT ${length} OFFSET ${offset}""".as1[String].toList
        }
      }
  }

  def get(id : String) : Option[Entry] = withSession(conn) { implicit session =>
    sql"""SELECT content FROM entries WHERE id=${id} AND pwn=0""".as1[String].headOption.map(_.parseJson.convertTo[Entry])
  }

  def next(id : String) : Option[String] = withSession(conn) { implicit session =>
    sql"""SELECT num FROM entries WHERE id=${id}""".as1[Int].headOption.flatMap({ num =>
      sql"""SELECT id FROM entries WHERE num=${num + 1} AND pwn=0""".as1[String].headOption
    })
  }
  def search(pattern : String) : List[(String, Entry)] = withSession(conn) { implicit session =>
    sql"""SELECT id, content FROM entries WHERE word LIKE ${pattern.replaceAll("\\*", "%")} AND pwn=0""".
      as2[String, String].map({ 
        case (id, content) => (id, content.parseJson.convertTo[Entry])
      }).toList
  }

  private def definitions(entry : Entry) : String = {
    entry.senses.map({sense => sense.definition}).mkString(";;;")
  }

  def update(id : String, entry : Entry) : Unit = withSession(conn) { implicit session =>
    sql"""UPDATE entries SET content=${entry.toJson.prettyPrint},
                             definition=${definitions(entry)} WHERE id=${id}""".execute
  }   

  def insert(entry : Entry, user : String) : String = withSession(conn) { implicit session =>
    val n : Int = sql"""SELECT max(num) FROM entries WHERE pwn=0""".as1[Int].head
    val idNew = Entrys.CWN_ENTRY + (n+2)
    sql"""INSERT INTO entries (id, word, definition, content, annotator, pwn) VALUES ($idNew, 
      ${entry.lemma}, ${entry.senses.map(_.definition).mkString(";;;")}, ${entry.toJson.toString},
      ${user}, 0)""".execute
    idNew
  }

  def find(s : String) : List[WordNetEntry] = withSession(conn) { implicit session =>
    sql"""SELECT id, word, definition FROM entries 
          WHERE definition != "" AND word LIKE ${s + "%"} 
          ORDER BY length(word) LIMIT 20""".as3[String, String, String].flatMap({
            case (id, word, defn) => defn.split(";;;").map({ defn =>
              WordNetEntry(word, id, defn)
            })
          }).toList
  }
}
