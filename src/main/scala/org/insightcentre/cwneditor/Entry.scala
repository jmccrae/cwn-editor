package org.insightcentre.cwneditor

case class Entry(lemma : String, val examples : List[Example],
    status : String, senses : List[Sense]) {
  def addExample(example : String) = Entry(lemma, Example(example) :: examples, status, senses)

  def validEntry = status match {
    case "" => false
    case "general" => true
    case "novel" => true
    case "vulgar" => true
    case _ => false
  }

  def abbrevOrMisspell = status == "abbrev" || status == "misspell"
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
