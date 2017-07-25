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

class CWNEditorServlet extends ScalatraServlet with ScalateSupport {
    import CWNEditorJsonProtocol._
    
    lazy val context = io.Source.fromFile("context").mkString.trim

    def urldecode(s : String) = java.net.URLDecoder.decode(s, "UTF-8")

    lazy val store = new SQLDataStore(new File("cwn.db"))
    lazy val login = SQLLogin
    lazy val activeUsers = new TimedHash()

    def file2entry(f : File) = {
      val s = io.Source.fromFile(f)
      val e = s.mkString.parseJson.convertTo[Entry]
      s.close
      e
    }

    def loggedin = {
      session.get("login") match {
        case Some(login) =>
          activeUsers.get(login.toString) != None
        case _ =>
          false
      }
    }

    get("/search") {
      params.get("lemma") match {
        case Some(lemma) =>
          val k = urldecode(lemma)
          val files = store.search(k).map({
            case (id, e) => (e, id)
          })
          contentType = "text/html"
          ssp("/summary",
            "files" -> files,
            "contextUrl" -> context,
            "loggedin" -> loggedin,
            "search" -> true)
        case None =>
          contentType = "text/html"
          ssp("/error",
            "contextUrl" -> context,
            "loggedin" -> loggedin,
            "message" -> "No query for search")
      }
    }


    get("/wn/:key") {
      val k = urldecode(params("key"))
      if(k.length >= 2) {
        val results = store.find(k)
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
          (store.get(f).get, f)
      })
      contentType = "text/html"
      ssp("/summary",
        "files" -> files,
        "next" -> (page + 1).toString,
        "contextUrl" -> context,
        "loggedin" -> loggedin)
    }

    get("/edit/:id") {
      try {
        session.get("login") match {
          case Some(login) =>
            val userName = activeUsers.get(login.toString).getOrElse(throw new EditorServletException("Session expired"))
            val id = params("id")
            store.get(id) match {
              case Some(data) =>
                contentType = "text/html"
                ssp("/edit", 
                  "error" -> params.get("error"),
                  "entryId" -> params("id"),
                  "entry" -> data,
                  "contextUrl" -> context,
                  "loggedin" -> true)
              case None =>
                pass()
            }
          case None =>
            SeeOther(context + "/login?redirect=/edit/"+params("id"))
        }
      } catch {
        case EditorServletException(msg) =>
          BadRequest(msg)
      }
    }

    def findNext(id : String) = store.next(id)

    get("/logout") {
      session.get("login").map({ login =>
        activeUsers.clear(login.toString)
      })
      SeeOther(context + "/")
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
        session.get("login") match {
          case Some(login) =>
            val userName = activeUsers.get(login.toString).getOrElse(throw new EditorServletException("Session expired"))
            val data = store.get(params("id")).getOrElse(throw new EditorServletException("ID does not exist"))
            val lemma = params.getOrElse("lemma", throw new EditorServletException("Lemma is required"))
            val status = params.getOrElse("status", throw new EditorServletException("Status is required"))
            val senseIds = params.keys.filter(_.matches("definition\\d+")).map({
              s => s.drop("definition".length).toInt
            })
            val e = Entry(lemma, data.examples, status, senseIds.map({ id =>
              val pos = params.getOrElse("pos" + id, throw new EditorServletException("POS is required"))
              // TODO: Account for abbreviation/misspelling
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
            val id = store.get(params("id"))
            TemporaryRedirect(context + "/login/?redirect=/update/"+id)
        }
      } catch {
        case EditorServletException(msg) => {
          TemporaryRedirect(context + "/edit/" + params("id") + "?error=" + msg)
        }
      }
    }

    post("/login/login") {
      try {
        val username = params.getOrElse("username", throw new EditorServletException("Username is required"))
        val password = params.getOrElse("password", throw new EditorServletException("Password is required"))
        val redirect = params.getOrElse("redirect", "/")
        login.login(username, password) match {
          case Some(key) =>
            activeUsers.put(key, username)
            session("login") = key
            SeeOther(context + redirect)
          case None =>
            SeeOther(context + "/login?auth_failure=1")
        }
      } catch {
        case EditorServletException(msg) => {
          BadRequest(msg)
        }
      }
    }

    post("/login/update") {
      try {
        val username = params.getOrElse("username", throw new EditorServletException("Username is required"))
        val oldPassword = params.getOrElse("oldpassword", throw new EditorServletException("Old password is required"))
        val newPassword = params.getOrElse("newpassword", throw new EditorServletException("New password is required"))
        if(login.updateUser(username, oldPassword, newPassword)) {
          SeeOther(context)
        } else {
          BadRequest("User update failed")
        }
      } catch {
        case EditorServletException(msg) => {
          BadRequest(msg)
        }
      }
    }

    post("/login/add") {
      try {
        val username = params.getOrElse("username", throw new EditorServletException("Username is required"))
        val password = params.getOrElse("password", throw new EditorServletException("Password is required"))
        val password2 = params.getOrElse("password2", throw new EditorServletException("Password2 is required"))
        val email = params.getOrElse("email", throw new EditorServletException("Email is required"))
        if(password == password2 && login.addUser(username, password, email)) {
          SeeOther(context)
        } else {
          BadRequest("User creation failed")
        }
      } catch {
        case EditorServletException(msg) => {
          BadRequest(msg)
        }
      }
    }


    get("/login") {
        val redirect = params.getOrElse("redirect","/")
        contentType = "text/html"
        ssp("/login", "redirect" ->  redirect, "contextUrl" -> context, "loggedin" -> loggedin)
    }

    get("/sign_up") {
        val redirect = params.getOrElse("redirect","/")
        contentType = "text/html"
        ssp("/sign_up", "redirect" ->  redirect, "contextUrl" -> context, "loggedin" -> loggedin)
    }

    get("/update_user") {
        val redirect = params.getOrElse("redirect","/")
        contentType = "text/html"
        ssp("/update_user", "redirect" ->  redirect, "contextUrl" -> context, "loggedin" -> loggedin)
    }

    get("/") {
      TemporaryRedirect(context + "/summary/0")
    }
}

case class EditorServletException(msg : String) extends RuntimeException(msg)
