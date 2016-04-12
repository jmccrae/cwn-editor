package org.insightcentre.cwneditor

case class Entry(lemma : String, examples : List[Example],
    status : String, senses : List[Sense])

case class Example(text : String)

case class Sense(definition : String,
    synonym : String,
    relations : List[Relation],
    id : Int)

case class Relation(`type` : String, target : String, relNo : Int)
