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
  implicit val entryFormat = jsonFormat6(Entry)
}

class CWNEditorServlet extends ScalatraServlet with ScalateSupport {
    import CWNEditorJsonProtocol._
    
    lazy val context = io.Source.fromFile("context").mkString.trim

    def urldecode(s : String) = java.net.URLDecoder.decode(s, "UTF-8")

    lazy val store = new SQLDataStore(new File("cwn.db"))
    lazy val login = SQLLogin
    lazy val activeUsers = collection.mutable.HashMap[String, String]()
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
      val recency = params.contains("recent")
      val annotator = params.get("annotator") match {
        case Some(a) => Some(a)
        case None => if(params.contains("annotator_me")) {
          username
        } else {
          None
        }
      }
          
      val files = store.listRange(page * 100, 100, recency, annotator).map({
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
        activeUsers.remove(login.toString)
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
      val id = params("id")
      if(params("id").startsWith(Entrys.CWN_NEW))
        updateNew(id)
      else
        updateExisting(id)
    }

    def updateExisting(id : String) : Any = {
      val entry = Try(store.get(id).getOrElse(throw new EditorServletException("ID invalid")))
      username match {
        case Some(userName) =>
          entryFromParams(id, entry) match {
            case Success(e) =>
              store.update(id, e)
              findNext(id) match {
                case Some(id) => TemporaryRedirect(s"$context/edit/$id")
                case None => TemporaryRedirect(s"$context/")
              }
            case Failure(Skip) => 
              findNext(id) match {
                case Some(id) => TemporaryRedirect(s"$context/edit/$id")
                case None => TemporaryRedirect(s"$context/")
              }
            case Failure(EntryValidityException(msg, e)) =>
              store.update(id, e)
              TemporaryRedirect(s"$context/edit/$id?error=$msg")
            case Failure(e) =>
              TemporaryRedirect(s"$context/edit/$id?error=${e.getMessage}")
          }
        case None =>
          TemporaryRedirect(s"$context/login?redirect=/update/$id")
      }
    }

    def updateNew(id : String) : Any = {
      Try(id.drop(Entrys.CWN_NEW.length).toInt) match {
        case Success(annoId) => {
          val entry = Try(annoQueue.get(annoId).
            getOrElse(throw new EditorServletException("ID not in queue")).toEntry)
          username match {
            case Some(userName) =>
              entryFromParams("NEW_ENTRY", entry) match {
                case Success(e) => 
                  store.insert(e, userName)
                  annoQueue.remove(userName, annoId)
                  annoQueue.getQueue(userName).headOption match {
                    case Some(aqe) =>
                      TemporaryRedirect(s"$context/add/${aqe.id}")
                    case None =>
                      TemporaryRedirect(s"$context/")
                  }
                case Failure(Skip) =>
                  annoQueue.remove(userName, annoId)
                  annoQueue.getQueue(userName).headOption match {
                    case Some(aqe) =>
                      TemporaryRedirect(s"$context/add/${aqe.id}")
                    case None =>
                      TemporaryRedirect(s"$context/")
                  }
                case Failure(EntryValidityException(msg, e)) =>
                  val newId = store.insert(e, userName)
                  annoQueue.remove(userName, annoId)
                  TemporaryRedirect(s"$context/edit/${newId}?error=$msg")
                case Failure(e) =>
                  TemporaryRedirect(s"$context/add/$annoId?error=${e.getMessage()}")
              }
            case None =>
              TemporaryRedirect(s"$context/login?redirect=/add/$annoId")
          }
        }
        case Failure(_) =>
          contentType = "text/html"
          ssp("/error",
            "contextUrl" -> context,
            "loggedin" -> loggedin,
            "message" -> "ID is invalid")
        }
    }

    def reduceTries[A](s : Seq[Try[A]]) : Try[Seq[A]] = s.foldLeft(Try(Seq[A]())) { (s, x) =>
      s.flatMap { s =>
        x.flatMap { x =>
          Success(s :+ x)
        }
      }
    }

    def entryFromParams(editorId : String, entry : Try[Entry]) : Try[Entry] = {
      entry flatMap { entry =>
        Try(params.getOrElse("confidence", 
          throw new EntryValidityException("Confidence is required", entry)
        )) flatMap { confidence =>
          if(confidence == "skip") {
            Failure(Skip)
          } else {
            Try(params("lemma")) flatMap { lemma =>
              Try(params.getOrElse("status", 
                throw new EntryValidityException("Status is required", entry.copy(confidence=confidence))
              )) flatMap { status =>
                if(status == "general" || status == "novel" || status == "vulgar") {
                  val senseIds = params.keys.filter(_.matches("definition\\d+")).map({ 
                    s => s.drop("definition".length).toInt
                  }).toSeq
                  val senses = senseIds.map({ id =>
                    Try(params.getOrElse("pos"+id, 
                        throw new EntryValidityException("Part of speech is required", entry.copy(confidence=confidence,status=status))
                    )) flatMap { pos =>
                      val defnOpt = params.get("definition" + id)
                      Try(defnOpt.getOrElse(throw new EditorServletException("Definition missing"))) flatMap { defn =>
                        Try(params("synonym"+id)) flatMap { synonym =>
                          if((defn != "" && synonym != "") || (defn == "" && synonym == "")) {
                            Failure(new EntryValidityException("Please set either definition or synonym, not both", entry.copy(confidence,status=status)))
                          } else {
                            val relIds = params.keys.filter(_.matches("relType" + id + "-\\d+")).map({
                              s => s.drop("relType".length + id.toString.length + 1).toInt
                            }).toSeq
                            val rels = relIds.map({ rid  =>
                              Try(params(s"relType$id-$rid")) flatMap { relType =>
                                Try(params(s"relTarget$id-$rid")) flatMap { relTarget =>
                                  Success(Relation(relType, relTarget, rid))
                                }
                              }
                            }).filter(_.map(_.`type` != "none").getOrElse(true))
                            reduceTries(rels) flatMap { rels =>
                              Success(Sense(pos, defn, synonym, rels.toList, id))
                            }
                          }
                        }
                      }
                    }
                  })
                  reduceTries(senses) flatMap { senses =>
                    Success(Entry(lemma, confidence, entry.examples, status, senses.toList, editorId))
                  }
                } else if(status == "abbrev") {
                  val abbrevs = params.keys.filter(_.matches("abbrev\\d+")).map(k => params(k))
                  Success(Entry(lemma, confidence, entry.examples, status, 
                    abbrevs.map(a => Sense("x", a, "", Nil, 1)).toList, editorId))
                } else if(status == "misspell") {
                  val abbrevs = params.keys.filter(_.matches("misspell\\d+")).map(k => params(k))
                  Success(Entry(lemma, confidence, entry.examples, status, 
                    abbrevs.map(a => Sense("x", a, "", Nil, 1)).toList, editorId))
                } else if(status == "inflected") {
                  val abbrevs = params.keys.filter(_.matches("inflected\\d+")).map(k => params(k))
                  Success(Entry(lemma, confidence, entry.examples, status, 
                    abbrevs.map(a => Sense("x", a, "", Nil, 1)).toList, editorId))
                } else {
                  Success(Entry(lemma, confidence, entry.examples, status, Nil, editorId))
                }
              }
            }
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
          val key = login.login(username, password).get
          activeUsers.put(key, username)
          session("login") = key
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

    get("/pull_queue/") {
      username match {
        case Some(username) =>
          Try(params("n").toInt) match {
            case Success(n) =>
              annoQueue.pullQueue(username, n)
              TemporaryRedirect(s"$context/")
            case Failure(_) =>
              pass()
          }
         case None =>
           TemporaryRedirect(s"$context/login?redirect=$context/pull_queue/?n=${params("n")}")
      }
    }

    get("/queue/") {
      username match {
        case Some(username) =>
          val queue = annoQueue.getQueue(username)
          contentType = "text/html"
          ssp("/queue", "queue" -> queue, "contextUrl" -> context, "loggedin" -> true)
        case None =>
          TemporaryRedirect(s"$context/login?redirect=$context/queue/")
      }
    }

          
    get("/") {
      username match {
        case Some(username) =>
          TemporaryRedirect(s"$context/queue/")
        case None =>
          TemporaryRedirect(s"$context/summary/0")
      }
    }
}

case class EditorServletException(msg : String) extends RuntimeException(msg)
case class EntryValidityException(msg : String, entry : Entry) extends RuntimeException(msg)
object Skip extends RuntimeException("Object skipped")
