package org.insightcentre.cwneditor
import spray.json._

object CWNEditorJsonProtocol extends DefaultJsonProtocol {
  implicit val synsetRelationFormat = jsonFormat2(SynsetRelation)
  implicit val relationFormat = jsonFormat3(Relation)
  implicit val synsetFormat = jsonFormat4(Synset)
  implicit val synsetWithMembersFormat = jsonFormat5(SynsetWithMembers)
  implicit val senseFormat = jsonFormat2(Sense)
  implicit val exampleFormat = jsonFormat1(Example)
  implicit val entryFormat = jsonFormat5(Entry)
}

case class Entry(val lemma : String, 
    val confidence : String,
    val examples : List[Example],
    status : String, senses : List[Sense]) {
  def addExample(example : String) = Entry(lemma, confidence,
    Example(example) :: examples, status, senses)

  def validEntry = status match {
    case "" => false
    case "general" => true
    case "novel" => true
    case "vulgar" => true
    case _ => false
  }

  def abbrevOrMisspell = status == "abbrev" || status == "misspell"
  def updateIds(map : Map[String, String]) : Entry = this.copy(
      senses = senses.map(s => Sense(s.relations, map.getOrElse(s.synset, s.synset))))
}

object Entrys {
  val CWN_NEW = "cwn-new-"
  val CWN_ENTRY = "cwn-entry-"
  val DEFN_REGEX = "(.*): (.*) <(.*)>".r
}

case class Example(text : String)

case class Sense(
    relations : List[Relation],
    synset : String) {
}

case class Synset(
  id : String,
  pos : String,
  definition : String,
  relations : List[SynsetRelation]
) {
    if(pos == "") throw new IllegalArgumentException()
  def partOfSpeech = pos match {
    case "n" => "noun"
    case "v" => "verb"
    case "a" => "adjective"
    case "r" => "adverb"
    case "x" => "other"
  }
}

case class SynsetWithMembers(
  id : String,
  pos : String,
  definition : String,
  relations : List[SynsetRelation],
  lemmas : List[String])

case class Relation(`type` : String, trgWord : String, trgSynset : String) {
  if(`type` == "") throw new IllegalArgumentException();
}

case class SynsetRelation(`type` : String, trgSynset : String) 
