package org.insightcentre.cwneditor

import spray.json._
import spray.json.DefaultJsonProtocol._



case class WordNetEntry(word : String, ili : String, definition : String)

class StringTrie[B](val map : Map[Char, StringTrie[B]], val values : List[B]) {
  def find(s : String) : List[B] = {
    s match {
      case "" =>
        values ++ map.values.flatMap(_.find(""))
      case s => 
        map.get(s.charAt(0)) match {
          case Some(st) =>
            st.find(s.drop(1))
          case None =>
            Nil
        }
    }
  }
  override def toString = s"""{[${values.mkString(", ")}], ${map.toString}}"""
}

class MutStringTrie[B]() {
  val map    = collection.mutable.Map[Char, MutStringTrie[B]]()
  val values = collection.mutable.ListBuffer[B]()
  def build : StringTrie[B] = new StringTrie(map.mapValues(_.build).toMap, values.toList)
  def insert(value : B, key : String) {
    key match {
      case "" =>
        values += value
      case _ => map.get(key.charAt(0)) match {
        case Some(s) =>
          s.insert(value, key.drop(1))
        case None =>
          val t = new MutStringTrie[B]()
          t.insert(value, key.drop(1))
          map.put(key.charAt(0), t)
      }
    }
  }
}

class WordNet {
  private def trie[B](values : Iterator[(String, B)]) : StringTrie[B] = {
    val s = new MutStringTrie[B]()
    for((key, value) <- values) {
      s.insert(value, key)
    }
    s.build
  }
//  private def multiMap[A,B](values : Iterator[(A,B)]) : Map[A, Set[B]] = {
//    values.toIterable.groupBy(_._1).mapValues(_.map(_._2).toSet)
//  }

  val data : StringTrie[WordNetEntry] = {
    trie(io.Source.fromInputStream(
      new java.util.zip.GZIPInputStream(
        new java.io.FileInputStream("data/wn31.csv.gz"))).getLines.map({ line =>
      val e = line.split(",")
      e(0) -> WordNetEntry(e(0),e(1),e.drop(2).mkString(","))
    }))
  }
}
