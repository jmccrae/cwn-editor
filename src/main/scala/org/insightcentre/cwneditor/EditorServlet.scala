package org.insightcentre.cwneditor

import java.io.File
import org.scalatra._
import scalate.ScalateSupport
import spray.json._

object CWNEditorJsonProtocol extends DefaultJsonProtocol {
  implicit val relationFormat = jsonFormat3(Relation)
  implicit val senseFormat = jsonFormat4(Sense)
  implicit val exampleFormat = jsonFormat1(Example)
  implicit val entryFormat = jsonFormat4(Entry)
}

class CWNEditorServlet extends ScalatraServlet with ScalateSupport {
    import CWNEditorJsonProtocol._

    def urldecode(s : String) = java.net.URLDecoder.decode(s, "UTF-8")

    lazy val wordnet = new WordNet()

    get("/wn/:key") {
      val k = urldecode(params("key"))
      if(k.length >= 3) {
        val results = wordnet.data.find(k)
        contentType = "application/javascript"
        "[" + results.map({ result =>
          s""""${result.word}: ${result.definition.replaceAll("\\\"","'")} <${result.ili}>""""
        }).mkString(",") + "]"
      } else {
        ""
      }

    }

    get("/edit/:id") {
        val f = new File("data/%s.json" format params("id"))
        if(!f.exists) {
            pass()
        } else {
            val data = io.Source.fromFile(f).mkString.parseJson.convertTo[Entry]
            contentType = "text/html"
            mustache("/edit", 
                "entry" -> params("id"),
                "lemma" -> data.lemma,
                "status" -> data.status,
                "examples" -> data.examples,
                "senses" -> data.senses)
        }
    }

    get("/update/:id") {
      val lemma = params.getOrElse("lemma", throw new RuntimeException())
      val status = params.getOrElse("status", throw new RuntimeException())
      val senseIds = params.keys.filter(_.matches("definition\\d+")).map({
        s => s.drop("definition".length).toInt
      })
      val e = Entry(lemma, Nil, status, senseIds.map({ id =>
        val definition = params.getOrElse("definition" + id, throw new RuntimeException())
        val synonym = params.getOrElse("synonym" + id, throw new RuntimeException())
        val relIds = params.keys.filter(_.matches("relType" + id + "-\\d+")).map({
          s => s.drop("relType".length + id.toString.length + 1).toInt
        })
        Sense(definition, synonym, relIds.map({ rid =>
          Relation(params.getOrElse("relType" + id + "-" + rid, throw new RuntimeException()),
                   params.getOrElse("relTarget" + id + "-" + rid, throw new RuntimeException()),
                   rid)
        }).toList, id)
      }).toList)
      contentType = "text/plain"
      e.toString
    }


    get("/") {
        contentType="text/html"
        mustache("/index")
    }
}
