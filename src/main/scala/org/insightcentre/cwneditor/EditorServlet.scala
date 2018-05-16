package org.insightcentre.cwneditor

import java.io.File
import spray.json._
import org.scalatra._
import scalate.ScalateSupport
import scala.collection.JavaConversions._
import scala.util.{Try, Success, Failure}

class CWNEditorServlet extends ScalatraServlet with ScalateSupport {
    import CWNEditorJsonProtocol._
    
    lazy val context = io.Source.fromFile("context").mkString.trim

    def urldecode(s : String) = java.net.URLDecoder.decode(s, "UTF-8")

    lazy val store = new db.DB(new File("cwn.db"))
    lazy val login = store
    lazy val activeUsers = collection.mutable.HashMap[String, String]()
    lazy val annoQueue = store

    def file2entry(f : File) = {
      val s = io.Source.fromFile(f)
      val e = s.mkString.parseJson.convertTo[Entry]
      s.close
      e
    }

    def username : Option[String] = session.get("login").flatMap(login => activeUsers.get(login.toString))

    def loggedin = username != None

    def getSynsets(syns : Seq[String]) : Map[String, SynsetWithMembers] = {
      syns.toSet.flatMap((s: String) =>
          store.getSynsetWithMembers(s).map(x => s -> x)).toMap
    }

    get("/json/entry/:name") {
      store.getEntry(params("name")) match {
        case Some(entry) =>
          contentType = "application/json"
          entry.toJson.toString
        case None =>
          NotFound(params("name"))
      }
    }

    get("/json/synset/:name") {
      store.getSynset(params("name")) match {
        case Some(synset) =>
          contentType = "application/json"
          synset.toJson.toString
        case None =>
          NotFound(params("name"))
      }
    }

    get("/synset/:name") {
      store.getSynset(params("name")) match {
        case Some(synset) =>
          contentType = "text/html"
          val entries = store.getSynsetEntries(params("name"))
          val synsets = getSynsets(
            synset.relations.map(_.trgSynset) ++
            entries.flatMap(_.senses.flatMap(_.relations.map(_.trgSynset))))
          ssp("/synset",
              "contextUrl" -> context,
              "synset" -> synset,
              "synsets" -> synsets,
              "entries" -> entries,
              "loggedin" -> loggedin)
        case None =>
          pass
      }
    }

    get("/entry/:name") {
       store.getEntry(params("name")) match {
          case Some(entry) =>
            val synsets = getSynsets(entry.senses.map(_.synset) ++
              entry.senses.flatMap(_.relations.map(_.trgSynset)) ++
              entry.senses.flatMap(s => store.getSynset(s.synset).toSeq.flatMap(ss =>
                  ss.relations.map(_.trgSynset))))
            contentType = "text/html"
            ssp("/entry",
                "contextUrl" -> context,
                "entry" -> entry,
                "synsets" -> synsets,
                "loggedin" -> loggedin)
            case None =>
              // TODO: Add to queue
              pass
        }
   }

    get("/edit/:name") {
      username match {
        case Some(username) => {
          store.getEntry(params("name")) match {
           case Some(entry) =>
             val synsets = getSynsets(entry.senses.map(_.synset))
             contentType = "text/html"
             ssp("/edit",
               "next" -> params.get("next"),
               "contextUrl" -> context,
               "entry" -> entry,
               "synsets" -> synsets,
               "loggedin" -> loggedin)
           case None =>
             // TODO: Add to queue
             pass
          }
        }
        case None =>
          TemporaryRedirect(s"$context/login?redirect=$context/edit/${params("name")}")
      }
    }


    get("/wn/:key") {
      val k = urldecode(params("key"))
      if(k.length >= 1) {
        val results = store.find(k)
        contentType = "application/javascript"
        "[" + results.map({ result =>
          s""""${result.word} (${result.pos}): ${result.definition.replaceAll("\\\"","'")} <${result.ili}>""""
        }).mkString(",") + "]"
      } else {
        ""
      }

    }

    val DISPLAY_STATUSES = Set("general", "novel", "vulgar", "abbrev", 
                        "misspell", "inflected")

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
      }).filter(x => DISPLAY_STATUSES contains x._1.status)
      val syns = files.map(_._1).flatMap({ e =>
        if(e.validEntry) { e.senses.map(_.synset) } else { Nil }
      }).toSet.flatMap((s:String) => store.getSynset(s).map(x => s -> x)).toMap
      contentType = "text/html"
      ssp("/summary",
        "files" -> files,
        "synsets" -> syns,
        "next" -> (page + 1).toString,
        "contextUrl" -> context,
        "loggedin" -> loggedin)
    }

    def acceptUpdate(username : String, next : Option[String]) = {
      next match {
        case Some(s) =>
          store.removeOrReview(username, s.toInt)
        case None =>
      }
      Ok("ok")
    }

    post("/update/:lemma") {
      import client.CWNClientEditorProtocol._
      try {
        username match {
          case Some(username) => {
            try {
              Try(request.body.parseJson.convertTo[client.Entry]) match {
                case Success(clientEntry) => {
                  clientEntry.confidence match {
                    case "skip" => 
                      acceptUpdate(username, params.get("next"))
                    case s if Set("vstrong", "strong", "medium", "weak") contains s => {
                      clientEntry.status match {
                        case s if Set("general", "novel", "vulgar", "abbrev", 
                          "misspell", "inflected", "name", "nonlex", "error") contains s => {
                          clientEntry.toDBEntry match {
                            case Success((_entry, synsets)) => {
                              val smap = synsets.map({ synset =>
                                synset.id -> store.updateSynset(synset.id, synset)
                              }).toMap
                              val entry = _entry.updateIds(smap)
                              store.update(username, params("lemma"), entry)
                              store.updateEntrySynset(params("lemma"), entry, synsets.map(ss =>
                                  ss.copy(id = smap(ss.id))))
                              val resp = acceptUpdate(username, params.get("next"))
                              if(clientEntry.status == "inflected") {
                                for(alt <- clientEntry.inflecteds) {
                                  if(alt.`new` == Some(true)) {
                                    store.assign(username,
                                      alt.text,
                                      clientEntry.examples.map(_.text))

                                  }
                                }
                              }
                              if(clientEntry.status == "misspell") {
                                for(alt <- clientEntry.misspells) {
                                  if(alt.`new` == Some(true)) {
                                    store.assign(username, alt.text,
                                      clientEntry.examples.map(_.text))
                                  }
                                }
                              }
                              if(clientEntry.status == "abbrev") {
                                for(alt <- clientEntry.abbrevs) {
                                  if(alt.`new` == Some(true)) {
                                    store.assign(username, alt.text,
                                      clientEntry.examples.map(_.text))
                                  }
                                }
                              }
                              resp
                            }
                            case Failure(reason) =>
                              BadRequest("Failed to convert to DB:" + reason.getMessage)
                          }
                        }
                        case _ => BadRequest("Status is not set")
                      }
                    }
                    case _ =>
                      BadRequest("Confidence is not set")
                  }
                }
                case Failure(reason) => {
                  System.err.println("Could not convert object")
                  BadRequest(reason.getMessage)
                }
              }
            } catch {
              case x : Exception => 
                x.printStackTrace()
                InternalServerError(x.getMessage())
            }
          }
          case None =>
            BadRequest("Session expired")
        }
      } catch {
        case x : Exception =>
          x.printStackTrace()
          InternalServerError(x.getMessage())
      }
    }

    get("/search") {
      params.get("lemma") match {
        case Some(lemma) =>
          val k = urldecode(lemma)
          val files = store.search(k).map({
            case (id, e) => (e, id)
          })
          val syns = files.map(_._1).flatMap({ e =>
            if(e.validEntry) { e.senses.map(_.synset) } else { Nil }
          }).toSet.flatMap((s:String) => store.getSynset(s).map(x => s -> x)).toMap
          contentType = "text/html"
          ssp("/summary",
            "files" -> files,
            "synsets" -> syns,
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

    get("/logout") {
      session.get("login").map({ login =>
        activeUsers.remove(login.toString)
      })
      TemporaryRedirect(context + "/")
    }

    get("/next/:id") {
      store.next(params("id")) match {
        case Some(id) => TemporaryRedirect(context + "/edit/" + id)
        case None => {
          contentType = "text/plain"
          "No more results"
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
            SeeOther(redirect)
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
        val reviewer = params.getOrElse("reviewer", "")
        if(password == password2 && login.addUser(username, password, email, reviewer)) {
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
      username match {
        case Some(username) => {
          val redirect = params.getOrElse("redirect","/")
          contentType = "text/html"
          ssp("/update_user", "redirect" ->  redirect, "contextUrl" -> context, "loggedin" -> loggedin)
        }
        case None => {
          TemporaryRedirect(s"$context/login?redirect=$context/update_user")
        }
      }
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
                val lemma = if(data.lemma.startsWith("*")) { 
                  data.lemma.drop(1)
                } else {
                  data.lemma
                }
                store.getEntry(lemma) match {
                  case Some(entry) =>
                    store.update(username, lemma, 
                      entry.copy(examples=data.examples.map(Example)))
                  case None =>
                    store.addEntry(lemma, data.examples)
                }
                TemporaryRedirect(s"$context/edit/${lemma}?next=$id")
              case None =>
                pass()
              }
          case None =>
            TemporaryRedirect(s"$context/login?redirect=$context/add/$id")
        }
      })
    }

    get("/next_queue") {
      username match {
        case Some(username) =>
          store.getQueue(username).headOption match {
            case Some(aqe) =>
              TemporaryRedirect(s"$context/add/${aqe.id}")
            case None =>
              TemporaryRedirect(s"$context/queue/")
          }
        case None =>
          TemporaryRedirect(s"$context/login?redirect=$context/next_queue")
      }
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
