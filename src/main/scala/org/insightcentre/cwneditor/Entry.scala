package org.insightcentre.cwneditor

case class Entry(lemma : String, val examples : List[Example],
    status : String, senses : List[Sense]) {
  def addExample(example : String) = Entry(lemma, Example(example) :: examples, status, senses)
}

case class Example(text : String)

case class Sense(
    pos : String,
    definition : String,
    synonym : String,
    relations : List[Relation],
    id : Int)

case class Relation(`type` : String, target : String, relNo : Int)
