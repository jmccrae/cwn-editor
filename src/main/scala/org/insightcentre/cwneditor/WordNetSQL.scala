package org.insightcentre.cwneditor

import java.io.File
import java.sql.DriverManager
import sql._

trait WordNet {
  def find(s : String) : List[WordNetEntry]
}

case class WordNetEntry(word : String, ili : String, definition : String)

case class WordNetSQL(dbURL : String) extends WordNet {

  def find(s : String) : List[WordNetEntry] = 
    withSession(DriverManager.getConnection(dbURL)) { implicit session =>
      sql"""SELECT word, ili, definition FROM wn WHERE word like ${s+"%"} ORDER BY word COLLATE NOCASE""".
        as3[String, String, String].map({
          case (x,y,z) => WordNetEntry(x,y,z)
        }).toList
    }
}

object WordNetSQL {
  try {
    Class.forName("org.sqlite.JDBC") }
  catch {
    case x : ClassNotFoundException => throw new RuntimeException("No Database Driver", x) }


  def data = {
    val f = new File("wn.db")
    val db = "jdbc:sqlite:wn.db"
    if(!f.exists) {
      val c = DriverManager.getConnection(db)
      c.setAutoCommit(false)
      withSession(c) { implicit session =>
        sql"""CREATE TABLE wn (word TEXT,
                               ili TEXT,
                               definition TEXT)""".execute
        sql"""CREATE INDEX wnidx ON wn (word)""".execute
        val insertWord = sql"""INSERT INTO wn VALUES (?, ?, ?)""".insert3[String, String, String]
        var i = 0
        System.err.print("Creating WordNet DB")
        io.Source.fromInputStream(
          new java.util.zip.GZIPInputStream(
            new java.io.FileInputStream("data/wn31.csv.gz"))).getLines.foreach({ line =>
          val e = line.split(",")
          insertWord(e(0), e(1), e.drop(2).mkString(","))
          i += 1
          if(i % 10000 == 0) {
            insertWord.execute
            System.err.print(".")
          }
        })
        insertWord.execute
        System.err.println("")
        c.commit()
      }
    }
    new WordNetSQL(db)
  }
}
