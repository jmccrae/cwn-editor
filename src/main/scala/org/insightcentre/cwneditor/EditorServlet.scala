package org.insightcentre.cwneditor

import java.io.File
import org.scalatra._
import scalate.ScalateSupport
import spray.json._
import scala.collection.JavaConversions._

object CWNEditorJsonProtocol extends DefaultJsonProtocol {
  implicit val relationFormat = jsonFormat3(Relation)
  implicit val senseFormat = jsonFormat5(Sense)
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

    get("/summary/:page") {
      val page = params("page").toInt
      val files = (for(f <- new File("data/").listFiles.sortBy(_.getName()).drop(page * 100).take(100)
        if f.getName().endsWith(".json")) yield {
          Map("name" -> f.getName().dropRight(5),
            "data" -> io.Source.fromFile(f).mkString.parseJson.convertTo[Entry])
        }).toList
      contentType = "text/html"
      mustache("/summary",
        "files" -> files)
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

    def findNext(id : String) = {
      val l1 = new File("data/").listFiles.sortBy(_.getName()).dropWhile({ f=>
        f.getName() != ("%s.json" format id)
      }).filter(_.getName().endsWith(".json"))
      if(l1.size >= 2) {
        Some(l1.tail.head.getName().dropRight(5))
      } else {
        None
      }
    }

    get("/next/:id") {
      findNext(params("id")) match {
        case Some(id) => TemporaryRedirect("/edit/" + id)
        case None => {
          contentType = "text/plain"
          "No more results"
        }
      }
    }

    get("/update/:id") {
      val f = new File("data/%s.json" format params("id"))
      val data = io.Source.fromFile(f).mkString.parseJson.convertTo[Entry]
      val lemma = params.getOrElse("lemma", throw new RuntimeException())
      val status = params.getOrElse("status", throw new RuntimeException())
      val senseIds = params.keys.filter(_.matches("definition\\d+")).map({
        s => s.drop("definition".length).toInt
      })
      val e = Entry(lemma, data.examples, status, senseIds.map({ id =>
        val pos = params.getOrElse("pos" + id, throw new RuntimeException())
        val definition = params.getOrElse("definition" + id, throw new RuntimeException())
        val synonym = params.getOrElse("synonym" + id, throw new RuntimeException())
        val relIds = params.keys.filter(_.matches("relType" + id + "-\\d+")).map({
          s => s.drop("relType".length + id.toString.length + 1).toInt
        })
        Sense(pos, definition, synonym, relIds.map({ rid =>
          Relation(params.getOrElse("relType" + id + "-" + rid, throw new RuntimeException()),
                   params.getOrElse("relTarget" + id + "-" + rid, throw new RuntimeException()),
                   rid)
        }).filter(_.`type` != "none").toList, id)
      }).toList)
      val out = new java.io.PrintWriter(f)
      out.println(e.toJson.prettyPrint)
      out.flush
      out.close
      findNext(params("id")) match {
        case Some(id) => TemporaryRedirect("/edit/" + id)
        case None => {
          contentType = "text/plain"
          "No more results"
        }
      }
    }


    get("/") {
      val id = new File("data/").listFiles.sortBy(_.getName()).
        filter(_.getName().endsWith(".json")).head.getName().dropRight(5)
      TemporaryRedirect("/edit/" + id)
    }
}
