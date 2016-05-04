package org.insightcentre.cwneditor

import spray.json._
import java.io.{File, PrintWriter}
import CWNEditorJsonProtocol._

object FromCSV {
  def decsv(string : String) : List[String] = {
    if(string == "") {
      return Nil
    } else if(string.startsWith("\"")) {
      val i = string.indexOf("\",", 1)
      return string.slice(1, i - 1) :: decsv(string.drop(i + 2))
    } else {
      val i = string.indexOf(",")
      if(i >= 0) {
        return string.take(i) :: decsv(string.drop(i  + 1))
      } else {
        return List(string)
      }
    }
  }

  def main(args : Array[String]) {
    var data = collection.mutable.Map[String,Entry]()
    var lastId = ""
    for(line <- io.Source.fromFile("/home/jmccrae/Downloads/Twitter frequencies - output.csv").getLines.drop(1)) {
      val elems = decsv(line)
      if(elems.length < 8) {
        System.err.println(line + " => " + elems.mkString("--"))
      } else {
        if(!elems(0).matches("\\d+\\.\\d+") && elems(0) != "") {
        } else if(elems(1) == "") {
          data.put(lastId, data(lastId) addExample elems(3))
        } else {
          lastId = elems(1)
          val relations = elems.drop(8).grouped(2).filter(_(0) != "").zipWithIndex.map({
            case (List(r, o), i) => 
              Relation(r, o, i)
            case (_, i) =>
              Relation("err", "err", i)
          }).toList
          data.put(lastId, Entry(
            elems(2), List(Example(elems(3))), 
            elems(4), List(Sense(
              elems(5),
              elems(7), elems(6), relations, 1))))
        }
      }
    }
    for((key, entry) <- data) {
      val f = new File("data/%5s.json" format key replaceAll(" ", "-"))
      val out = new PrintWriter(f)
      out.println(entry.toJson.prettyPrint)
      out.flush
      out.close
      
    }
  }
}
