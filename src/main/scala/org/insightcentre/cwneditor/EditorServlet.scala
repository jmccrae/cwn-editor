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
                "lemma" -> data.lemma,
                "status" -> data.status,
                "examples" -> data.examples,
                "senses" -> data.senses)
        }
    }


    get("/") {
        contentType="text/html"
        mustache("/index")
    }
}
