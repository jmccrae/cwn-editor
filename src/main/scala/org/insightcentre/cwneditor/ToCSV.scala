package org.insightcentre.cwneditor

import java.io.{File, PrintWriter}
import spray.json._
import scala.collection.JavaConversions._

object ToCSV {
    import CWNEditorJsonProtocol._
  def main(args : Array[String]) {
    val out = new PrintWriter("data.csv")
    for(f <- new File("data/").listFiles.sortBy(_.getName()) 
         if f.getName().endsWith(".json")) {
      val data = io.Source.fromFile(f).mkString.parseJson.convertTo[Entry]
      if(data.status != "") {
      out.println("%s,%s,%s,%s" format
        (f.getName().dropRight(5),
          data.lemma,
          data.status,
          data.senses.map(_.pos).mkString(";")))
      }

    }
    out.flush
    out.close
  }
}
