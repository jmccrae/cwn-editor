package org.insightcentre.cwneditor

import java.io.File
import spray.json._
import scala.collection.JavaConversions._
import sql._
import java.sql.DriverManager

trait DataStore {
  //def list : List[String]
  def listRange(offset : Int, length : Int) : List[String]
  def get(id : String) : Option[Entry]
  def next(id : String) : Option[String]
  def search(pattern : String) : List[(String, Entry)]
  def update(id : String, entry : Entry) : Unit
}

object FileDataStore extends DataStore {
    import CWNEditorJsonProtocol._
    def file2entry(f : File) = {
      val s = io.Source.fromFile(f)
      val e = s.mkString.parseJson.convertTo[Entry]
      s.close
      e
    }

  def list = new File("data/").listFiles.filter(_.getName().endsWith(".json")).map(_.getName().dropRight(5)).toList
  def listRange(offset : Int, length : Int) = list.sorted.drop(offset).take(length)
  def next(id : String) = list.sorted.dropWhile(_ != id).tail.headOption
  def get(id : String) = Some(file2entry(new File("data/%s.json" format id)))
  def search(pattern : String) = list.map(x => (x, get(x).get)).filter({
    case (id, e) => e.lemma.matches(pattern.replaceAll("\\*",".*"))
  })
  def update(id : String, entry : Entry) = {
    val out = new java.io.PrintWriter(new File(new File("data"), id + ".json"))
    out.println(entry.toJson.prettyPrint)
    out.flush
    out.close
  }
}

class SQLDataStore(db : File) extends DataStore with WordNet {
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
                                  pwn BOOLEAN)""".execute
      sql"""CREATE INDEX entries_id ON entries (id)""".execute
      sql"""CREATE INDEX entries_word ON entries (word)""".execute
      val insertWord = sql""" INSERT INTO entries VALUES (?,?,?,?,?,?)""".insert6[Int,String,String,String,String,Boolean]
      var i = 0
      System.err.println("Loading database")
      io.Source.fromInputStream(
        new java.util.zip.GZIPInputStream(
          new java.io.FileInputStream("data.csv.gz"))).getLines.foreach({ line =>
            val e = line.split("\\|\\|\\|")
            insertWord(e(0).toInt, e(1), e(2), e(3), e(4), e(5) == "1")
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

  def listRange(offset : Int, length : Int) : List[String] = withSession(conn) { implicit session =>
    sql"""SELECT id FROM entries WHERE pwn=0 LIMIT ${length} OFFSET ${offset}""".as1[String].toList
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
