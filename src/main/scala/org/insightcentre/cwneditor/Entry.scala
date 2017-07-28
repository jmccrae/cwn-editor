package org.insightcentre.cwneditor

case class Entry(val lemma : String, val examples : List[Example],
    status : String, senses : List[Sense], editorId : String) {
  def addExample(example : String) = Entry(lemma, Example(example) :: examples, status, senses, editorId)

  def validEntry = status match {
    case "" => false
    case "general" => true
    case "novel" => true
    case "vulgar" => true
    case _ => false
  }

  def abbrevOrMisspell = status == "abbrev" || status == "misspell"
}

object Entrys {
  val CWN_NEW = "cwn-new-"
  val CWN_ENTRY = "cwn-entry-"
}

case class Example(text : String)

case class Sense(
    pos : String,
    definition : String,
    synonym : String,
    relations : List[Relation],
    id : Int) {
  def isSyn = synonym != ""
}

case class Relation(`type` : String, target : String, relNo : Int)
