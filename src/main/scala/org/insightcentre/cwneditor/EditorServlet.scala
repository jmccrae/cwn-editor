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
  implicit val entryFormat = jsonFormat5(Entry)
}

class CWNEditorServlet extends ScalatraServlet with ScalateSupport with AuthenticationSupport {
    import CWNEditorJsonProtocol._
    
    lazy val context = io.Source.fromFile("context").mkString.trim

    def urldecode(s : String) = java.net.URLDecoder.decode(s, "UTF-8")

    lazy val store = new SQLDataStore(new File("cwn.db"))
    lazy val wordnet : WordNet = store

    def file2entry(f : File) = {
      val s = io.Source.fromFile(f)
      val e = s.mkString.parseJson.convertTo[Entry]
      s.close
      e
    }

    get("/search/:lemma") {
      val k = urldecode(params("lemma"))
      val files = store.search(k).map({
        case (id, e) => (e, id)
      })
      contentType = "text/html"
      ssp("/summary",
        "files" -> files,
        "contextUrl" -> context)
    }


    get("/wn/:key") {
      val k = urldecode(params("key"))
      if(k.length >= 2) {
        val results = wordnet.find(k)
        contentType = "application/javascript"
        "[" + results.map({ result =>
          s""""${result.word}: ${result.definition.replaceAll("\\\"","'")} <${result.ili}>""""
        }).reverse.mkString(",") + "]"
      } else {
        ""
      }

    }

    get("/summary/:page") {
      val page = params("page").toInt
      val files = store.listRange(page * 100, 100).map({
        f => 
          (store.get(f), f)
      })
      contentType = "text/html"
      ssp("/summary",
        "files" -> files,
        "next" -> (page + 1).toString,
        "contextUrl" -> context)
    }

    get("/edit/:id") {
      basicAuth
        val f = new File("data/%s.json" format params("id"))
        if(!f.exists) {
            pass()
        } else {
            val data = file2entry(f)
            contentType = "text/html"
            ssp("/edit", 
                "error" -> params.get("error"),
                "entryId" -> params("id"),
                "entry" -> data,
                "contextUrl" -> context)
        }
    }

    def findNext(id : String) = store.next(id)

    get("/logout") {
      Unauthorized("Logged out")
    }

    get("/next/:id") {
      findNext(params("id")) match {
        case Some(id) => TemporaryRedirect(context + "/edit/" + id)
        case None => {
          contentType = "text/plain"
          "No more results"
        }
      }
    }

    get("/update/:id") {
      try {
        basicAuth match {
          case Some(User(userName)) =>
            val data = store.get(params("id"))
            val lemma = params.getOrElse("lemma", throw new EditorServletException("Lemma is required"))
            val status = params.getOrElse("status", throw new EditorServletException("Status is required"))
            val senseIds = params.keys.filter(_.matches("definition\\d+")).map({
              s => s.drop("definition".length).toInt
            })
            val e = Entry(lemma, data.examples, status, senseIds.map({ id =>
              val pos = params.getOrElse("pos" + id, throw new EditorServletException("POS is required"))
              val definition = params.getOrElse("definition" + id, throw new EditorServletException("Definition is required"))
              val synonym = params.getOrElse("synonym" + id, throw new EditorServletException("Synonym is required"))
              val relIds = params.keys.filter(_.matches("relType" + id + "-\\d+")).map({
                s => s.drop("relType".length + id.toString.length + 1).toInt
              })
              Sense(pos, definition, synonym, relIds.map({ rid =>
                Relation(params.getOrElse("relType" + id + "-" + rid, throw new EditorServletException("Rel type missing")),
                         params.getOrElse("relTarget" + id + "-" + rid, throw new EditorServletException("Rel target missing")),
                         rid)
              }).filter(_.`type` != "none").toList, id)
            }).toList, userName)
            store.update(params("id"), e)
            findNext(params("id")) match {
              case Some(id) => TemporaryRedirect(context + "/edit/" + id)
              case None => {
                contentType = "text/plain"
                "No more results"
              }
            }
          case None =>
            BadRequest("Please authenticate")
        }
      } catch {
        case EditorServletException(msg) => {
          TemporaryRedirect(context + "/edit/" + params("id") + "?error=" + msg)
        }
      }
    }


    get("/") {
      basicAuth
      TemporaryRedirect(context + "/summary/0")
    }
}

case class EditorServletException(msg : String) extends RuntimeException(msg)
