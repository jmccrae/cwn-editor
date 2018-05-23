/// The model of an entry as used by the client
package org.insightcentre.cwneditor.client

import org.insightcentre.cwneditor.{Entry => SEntry, Example, Sense => SSense, 
  Synset => SSynset, Relation => SRelation, SynsetRelation => SSynsetRelation}
import spray.json._
import scala.util.{Try,Success,Failure}

object CWNClientEditorProtocol extends DefaultJsonProtocol {
  implicit val relationFormat = jsonFormat3(Relation)
  implicit val synsetFormat = jsonFormat3(Synset)
  implicit val alternativeFormat = jsonFormat2(Alternative)
  implicit val exampleFormat = jsonFormat1(Example)
  implicit val senseFormat = jsonFormat3(Sense)
  implicit val entryFormat = jsonFormat8(Entry)
}

case class Entry(val lemma : String,
  val confidence : String,
  val examples : List[Example],
  status : String, senses : List[Sense],
  abbrevs : List[Alternative],
  misspells : List[Alternative],
  inflecteds : List[Alternative]) {
    def toDBEntry : Try[(SEntry, Seq[SSynset])] = {
      if(!Set("vstrong","strong","medium","weak").contains(confidence)) {
        Failure(new RuntimeException("Bad confidence"))
      } else if(Set("name", "nonlex", "error").contains(status)) {
        Try((SEntry(lemma, confidence, examples, status, Nil), Nil))

      } else if(status == "abbrev") {
        Try((SEntry(lemma, confidence, examples, status, abbrevs.map(_.toDBSense)),
          Nil))
      } else if(status == "misspell") {
        Try((SEntry(lemma, confidence, examples, status, misspells.map(_.toDBSense)),
          Nil))
      } else if(status == "inflected") {
        Try((SEntry(lemma, confidence, examples, status, inflecteds.map(_.toDBSense)),
          Nil))
      } else if(!Set("general", "vulgar", "novel").contains(status)) {
        Failure(new RuntimeException("Bad status"))
      } else {
        Try((SEntry(lemma, confidence, examples, status, senses.map(_.toDBSense)),
          senses.map(_.toSynset)))
      }
    } 

}

case class Sense(
    id : Int,
    relations : List[Relation],
    synset : Synset) {
      def toDBSense : SSense = synset.id match {
        case None =>
          SSense(relations.flatMap(_.toDBSenseRel), "n" + id)
        case Some("") =>
          SSense(relations.flatMap(_.toDBSenseRel), "n" + id)
        case Some(s) =>
          SSense(relations.flatMap(_.toDBSenseRel), s)
      }
      def toSynset : SSynset = synset.id match {
        case None =>
          SSynset("n" + id, synset.getPos, synset.definition, 
            relations.flatMap(_.toDBSynRel))
        case Some("") =>
          SSynset("n" + id, synset.getPos, synset.definition, 
            relations.flatMap(_.toDBSynRel))
        case Some(s) =>
          SSynset(s, synset.getPos, synset.definition, 
            relations.flatMap(_.toDBSynRel))
      }

    }

case class Synset(
  id : Option[String],
  pos : Option[String],
  definition : String
) {
  def getPos = pos.getOrElse(throw new RuntimeException("Part of Speech is required"))
}

case class Alternative(text : String, `new` : Option[String]) {
  def toDBSense = SSense(Nil, text)
  def isNew = `new` match {
    case Some("") => false
    case None => false
    case _ => true
  }
}

object Relations {
  val SENSE_RELS = Set(
    "antonym",
    "derivation",
    "loanword")
  val SYNSET_RELS = Set(
    "hypernym",
    "hyponym",
    "instance_hypernym",
    "instance_hyponym",
    "emotion",
    "also",
    "causes",
    "domain_region",
    "domain_topic",
    "domain_usage",
    "similar",
    "mero_location",
    "mero_member",
    "mero_part",
    "mero_portion",
    "mero_substance",
    "holo_location",
    "holo_member",
    "holo_part",
    "holo_portion",
    "holo_substance",
    "pejorative")
}
case class Relation(`type` : String, trgWord : Option[String], trgSynset : String) {
  import Relations._
  def toDBSenseRel : Option[SRelation] = if(SENSE_RELS.contains(`type`)) {
    Some(SRelation(`type`, trgWord.getOrElse(throw new IllegalArgumentException(s"${`type`} does not have a word")), trgSynset))
  } else {
    None
  }

  def toDBSynRel : Option[SSynsetRelation] = if(SYNSET_RELS.contains(`type`)) {
    Some(SSynsetRelation(`type`, trgSynset))
  } else {
    None
  }
}
