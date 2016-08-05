package org.insightcentre.cwneditor

import java.io.File
import spray.json._
import scala.collection.JavaConversions._

trait DataStore {
  def list : List[String]
  def listRange(offset : Int, length : Int) : List[String]
  def get(id : String) : Entry
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
  def get(id : String) = file2entry(new File("data/%s.json" format id))
  def search(pattern : String) = list.map(x => (x, get(x))).filter({
    case (id, e) => e.lemma.matches(pattern.replaceAll("\\*",".*"))
  })
  def update(id : String, entry : Entry) = {
    val out = new java.io.PrintWriter(new File(new File("data"), id + ".json"))
    out.println(entry.toJson.prettyPrint)
    out.flush
    out.close
  }
}
