package org.insightcentre.cwneditor

import spray.json._
import java.io._
import java.util.zip._

object LegacyCWNEditorJsonProtocol extends DefaultJsonProtocol {
  implicit val relationFormat = jsonFormat3(Relation)
  implicit val senseFormat = jsonFormat5(Sense)
  implicit val exampleFormat = jsonFormat1(Example)
  implicit val entryFormat = jsonFormat6(Entry)
  implicit val legacyEntryFormat = jsonFormat5(LegacyEntry)
}


case class LegacyEntry(val lemma : String, val examples : List[Example],
    status : String, senses : List[Sense], editorId : String) {
  def update(id : String) : Entry = Entry(lemma, "vstrong", examples, status, senses, id)
}

object LegacyJSONtoCSVConverter {
  import LegacyCWNEditorJsonProtocol._

  private def definitions(entry : LegacyEntry) : String = {
    if(entry.status == "general" || entry.status == "novel" || entry.status == "vulgar") {
      entry.senses.map({sense => sense.definition}).mkString(";;;")
    } else {
      ""
    }
  }

  private def examples(entry : LegacyEntry) : String = {
    entry.examples.map(_.text).mkString(";;;")
  }

  def main(args : Array[String]) {
    val out = new PrintWriter(new GZIPOutputStream(new FileOutputStream("data.csv.gz")))
    val out2 = new PrintWriter(new GZIPOutputStream(new FileOutputStream("queue.csv.gz")))
    var num = 0
    new File("data-old/").listFiles.filter(_.getName().endsWith(".json")).map({ file =>
      val entry = io.Source.fromFile(file).mkString.parseJson.convertTo[LegacyEntry]
      if(entry.lemma == "") {
        println(file.getName())
      }
      entry.copy(lemma=entry.lemma.replaceAll("^\\*",""))
    }).sortBy(_.lemma.toLowerCase).foreach({ entry =>
      val id = s"""cwn-entry-${num+1}"""
      if(entry.status != "") {
        val entry2 = entry.update(id)
        out.println(s"""$num|||${id}|||${entry.lemma}|||${definitions(entry)}|||${entry2.toJson.toString}|||0""")
        num += 1
      } else {
        out2.println(s"""${entry.lemma}|||${examples(entry)}""")
      }
    })
    io.Source.fromInputStream(
      new java.util.zip.GZIPInputStream(
        new java.io.FileInputStream("data-old/wn31.csv.gz"))).getLines.foreach({ line =>
          val e = line.split(",")
          out.println(s"""$num|||${e(1)}|||${e(0)}|||${e.drop(2).mkString(",")}|||{}|||1""")
          num += 1
        })
    out.flush
    out.close
    out2.flush
    out2.close

  }
}

