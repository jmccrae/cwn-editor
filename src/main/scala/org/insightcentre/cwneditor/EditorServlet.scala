package org.insightcentre.cwneditor

import java.io.File
import org.scalatra._
import scalate.ScalateSupport
import spray.json._
import scala.collection.JavaConversions._
import scala.util.{Try, Success, Failure}

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
    lazy val annoQueue = SQLAnnotationQueue

    def file2entry(f : File) = {
      val s = io.Source.fromFile(f)
      val e = s.mkString.parseJson.convertTo[Entry]
      s.close
      e
    }

    def username : Option[String] = session.get("login").flatMap(login => activeUsers.get(login.toString))

    def loggedin = username != None

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
        username match {
          case Some(userName) =>
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
            TemporaryRedirect(context + "/login?redirect=/edit/"+params("id"))
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
      TemporaryRedirect(context + "/")
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
      val isNew = params("id").startsWith(Entrys.CWN_NEW)
      val id = params("id")
      try {
        username match {
          case Some(userName) =>
            val examples = if(isNew) {
              annoQueue.get(Try(id.drop(Entrys.CWN_NEW.length).toInt).
                getOrElse(throw new EditorServletException("ID not integer"))).
                getOrElse(throw new EditorServletException("ID not in queue")).
                examples.map(Example(_))
            } else {
              store.get(params("id")).getOrElse(throw new EditorServletException("ID does not exist")).examples
            }
            val lemma = params.getOrElse("lemma", throw new EditorServletException("Lemma is required"))
            val status = params.getOrElse("status", throw new EditorServletException("Status is required"))
            val senseIds = params.keys.filter(_.matches("definition\\d+")).map({
              s => s.drop("definition".length).toInt
            })
            val e = Entry(lemma, examples, status, senseIds.map({ id =>
              val pos = params.getOrElse("pos" + id, throw new EditorServletException("POS is required"))
              val definition = params.get("definition" + id).orElse(
                params.get("abbrev" + id).orElse(
                  params.get("misspell" + id))).getOrElse(throw new EditorServletException("Definition is required"))
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
            if(isNew) {
              val annoId = Try(id.drop(Entrys.CWN_NEW.length).toInt).
                getOrElse(throw new EditorServletException("ID not integer"))
              store.insert(e)
              annoQueue.dequeue(userName, annoId)
              annoQueue.getQueue(userName).headOption match {
                case Some(aqe) =>
                  TemporaryRedirect(s"$context/add/${aqe.id}")
                case None =>
                  TemporaryRedirect(s"$context/")
              }
            } else {
              store.update(params("id"), e)
              findNext(params("id")) match {
                case Some(id) => TemporaryRedirect(s"$context/edit/$id")
                case None => TemporaryRedirect(s"$context/")
              }
            }
          case None =>
            if(isNew) {
              val annoId = Try(id.drop(Entrys.CWN_NEW.length).toInt).
                getOrElse(throw new EditorServletException("ID not integer"))
              TemporaryRedirect(s"$context/login/?redirect=/add/$annoId")
            } else {
              TemporaryRedirect(s"$context/login/?redirect=/update/$id")
            }
        }
      } catch {
        case EditorServletException(msg) => {
          if(isNew) {
            val annoId = Try(id.drop(Entrys.CWN_NEW.length).toInt).
                getOrElse(throw new EditorServletException("ID not integer"))
            TemporaryRedirect(s"$context/add/$annoId?error=$msg")
          } else {
            TemporaryRedirect(s"$context/edit/$id?error=$msg")
          }
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
            SeeOther(context + "/login?auth_failure=1&redirect=" + redirect)
        }
      } catch {
        case EditorServletException(msg) => {
          BadRequest(msg)
        }
      }
    }

    post("/login/update") {
      try {
        username match {
          case Some(username) =>
            val oldPassword = params.getOrElse("oldpassword", throw new EditorServletException("Old password is required"))
            val newPassword = params.getOrElse("newpassword", throw new EditorServletException("New password is required"))
            if(login.updateUser(username, oldPassword, newPassword)) {
              SeeOther(context + "/")
            } else {
              BadRequest("User update failed")
            }
          case None =>
            SeeOther(context + "/login?redirect=" + context + "/update_user")
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
          SeeOther(context + "/")
        } else if(password != password2) {
          SeeOther(context + "/sign_up?err=Passwords do not match")
        } else {
          SeeOther(context + "/sign_up?err=Username exists")
        }
      } catch {
        case EditorServletException(msg) => {
          SeeOther(context + "/sign_up?err="+msg)
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
        val err = params.getOrElse("err", "")
        contentType = "text/html"
        ssp("/sign_up", "err" -> err,  "redirect" ->  redirect, "contextUrl" -> context, "loggedin" -> loggedin)
    }

    get("/update_user") {
        val redirect = params.getOrElse("redirect","/")
        contentType = "text/html"
        ssp("/update_user", "redirect" ->  redirect, "contextUrl" -> context, "loggedin" -> loggedin)
    }

    def id2int(f : Int => Any) = {
      Try(params("id").toInt) match {
        case Success(id) => f(id)
        case Failure(e) => {
          contentType = "text/html"
          BadRequest(ssp("/error",
            "message" -> e.getMessage(),
            "contextUrl" -> context,
            "loggedin" -> loggedin))
        }
      }
    }


    get("/add/:id") {
      id2int(id => {
        username match {
          case Some(username) =>
            annoQueue.get(id) match {
              case Some(data) =>
                contentType = "text/html"
                ssp("/edit", 
                  "error" -> params.get("error"),
                  "entryId" -> (Entrys.CWN_NEW + params("id")),
                  "entry" -> data.toEntry,
                  "contextUrl" -> context,
                  "loggedin" -> true)
              case None =>
                pass()
              }
          case None =>
            TemporaryRedirect(s"$context/login?redirect=$context/add/$id")
        }
      })
    }

    get("/dequeue/:id") {
      id2int(id => {
        username match {
          case Some(username) =>
            annoQueue.remove(username, id)
            TemporaryRedirect(context + "/")
          case None =>
            TemporaryRedirect(s"$context/login?redirect=$context/dequeue/$id")
        }
      })
    }

    get("/extend/:id") {
      id2int(id => {
        username match {
          case Some(username) =>
            annoQueue.extend(username, id)
            TemporaryRedirect(context + "/")
          case None =>
            TemporaryRedirect(s"$context/login?redirect=$context/extend/$id")
        }
      })
    }
          
    get("/") {
      username match {
        case Some(username) =>
          val queue = annoQueue.getQueue(username)
          contentType = "text/html"
          ssp("/queue", "queue" -> queue, "contextUrl" -> context, "loggedin" -> true)
        case None =>
          TemporaryRedirect(context + "/summary/0")
      }
    }
}

case class EditorServletException(msg : String) extends RuntimeException(msg)
