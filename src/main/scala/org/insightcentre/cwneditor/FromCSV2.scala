package org.insightcentre.cwneditor

import spray.json._
import java.io.{File, PrintWriter}
import CWNEditorJsonProtocol._

object FromCSV2 {
  import FromCSV.decsv

  def mapStatus(sc : String, name : String, nonLex : String) = {
    if(name == "y") {
      "name"
    } else if(nonLex == "y") {
      "nonlex"
    } else if(sc == "v") {
      "vulgar"
    } else if(sc == "n") {
      "novel"
    } else if(sc == "g") {
      "general"
    } else {
      ""
    }
  }

  def mapRels(elems : List[String]) : List[Relation] = {
    var i = 0
    var l = collection.mutable.ListBuffer[Relation]()
    if(elems(9) != "") {
      l += Relation("hypernym", elems(9), i)
      i += 1
    }
    if(elems(10) != "") {
      l += Relation("derived", elems(10), i)
      i += 1
    }
    if(elems(11) != "") {
      l += Relation("similar", elems(11), i)
      i += 1
    }
    if(elems(12) != "") {
      l += Relation("antonym", elems(12), i)
      i += 1
    }
    if(elems(13) != "") {
      l += Relation("pejorative", elems(13), i)
      i += 1
    }
    if(elems(14) != "") {
      l += Relation("emotion", elems(14), i)
      i += 1
    }
    l.toList
  }

  def main(args : Array[String]) {
    var data = collection.mutable.Map[String,Entry]()
    for(line <- io.Source.fromFile("/home/jmccrae/Downloads/Reddit-based terms for Colloquial WordNet - reddit-freqs.tsv.csv").getLines.drop(1)) {
      val elems = decsv(line)
      if(elems.length >= 16 && elems(4) != "") {
        val id = elems(3)
        if(data contains id) {
          data.put(id, data(id) addExample elems(2))
        } else {
          data.put(id, Entry(
            elems(1),
            List(Example(elems(2))),
            mapStatus(elems(4), elems(5), elems(6)),
            List(Sense(
              elems(7), elems(15),
              elems(8),
              mapRels(elems),1))))
        }
      } else if(elems.length < 16) {
        System.err.println(line)
      }
    }
    for((key, entry) <- data) {
      val f = new File("data/-----reddit-%s.json" format key)
      val out = new PrintWriter(f)
      out.println(entry.toJson.prettyPrint)
      out.flush
      out.close
    }
  }
}

