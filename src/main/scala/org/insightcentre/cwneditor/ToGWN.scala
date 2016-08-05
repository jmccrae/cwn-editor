package org.insightcentre.cwneditor

object ToGWN {
  sealed trait Auxiliary
  case class AuxiliaryILISynset(ili : String) extends Auxiliary
  case class AuxiliaryILISense(ili : String, lemma : String, pos : String) extends Auxiliary
  case class AuxiliaryWN30Synset(id : String) extends Auxiliary

  val iliRef = ".*<(i\\d+)>".r
  val wn31Ref = ".*<wn31.(.*)-\\d>".r
  val synsetRef = ".*<-+(.*)\\-(\\d)>".r
  val iliSenseRef = "(.*?):.*<(i\\d+)>".r
  val senseRef = "(.*?):.*<-+(.*)\\-(\\d)>".r

  val senseRelations = Set("antonym", "derivation", "loanword")

  def lemmaEscape(s : String) = s.replaceAll(" ", "_")

  def toGWN(entries : Seq[(String, Entry)]) = {
    val aux : Set[Auxiliary] = entries.flatMap(auxiliaries).toSet
    s"""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE LexicalResource SYSTEM "http://globalwordnet.github.io/schemas/WN-LMF-1.0.dtd">
<LexicalResource xmlns:dc="http://purl.org/dc/elements/1.1/">
  <Lexicon id="colloqwn"
           label="Colloquial WordNet derived from Twitter"
           language="en" 
           email="john@mccr.ae"
           license="https://creativecommons.org/licenses/by/4.0/"
           version="0.1-prerelease"
           citation=""
           url="">
${entries.flatMap(toGWNEntry).mkString("\n")}
${aux.map(toAuxEntry).mkString("\n")}
${entries.flatMap(toGWNSynset).mkString("\n")}
${aux.map(toAuxSynset).mkString("\n")}
  </Lexicon>
</LexicalResource>"""
  }

  def toAuxEntry(aux : Auxiliary) = aux match {
    case a : AuxiliaryILISynset => ""
    case a : AuxiliaryWN30Synset => ""
    case AuxiliaryILISense(ili, lemma, pos) => 
      s"""    <LexicalEntry id="${lemmaEscape(lemma)}-$pos" note="aux">
      <Lemma writtenForm="$lemma" partOfSpeech="$pos"/>
      <Sense id="$lemma-$ili" synset="$ili"/>
    </LexicalEntry>"""
  }

  def toAuxSynset(aux : Auxiliary) = aux match {
    case AuxiliaryILISynset(ili) =>
      s"""    <Synset id="$ili" ili="$ili" note="aux"/>"""
    case AuxiliaryWN30Synset(id) =>
      s"""    <Synset id="wn31:$id" ili="" note="aux"/>"""
    case a : AuxiliaryILISense => ""
  }

  def auxiliaries(entry : (String, Entry)) : Seq[Auxiliary] = entry match {
    case (id, Entry(_, _, _, senses, _)) =>
      senses.flatMap(senseAux)
  }

  def senseAux(sense : Sense) : Seq[Auxiliary] = {
    if(sense.synonym == "") {
      sense.relations.flatMap(senseRelAux(_,sense.pos))
    } else {
      sense.synonym match {
        case iliRef(ili) => Seq(AuxiliaryILISynset(ili))
        case wn31Ref(id) => Seq(AuxiliaryWN30Synset(id))
        case _ => Nil
      }
    }
  }

  def senseRelAux(r : Relation, pos : String) : Seq[Auxiliary] = 
    if(senseRelations contains r.`type`) {
      r.target match {
        case iliSenseRef(lemma, ili) => Seq(AuxiliaryILISense(ili, lemma, pos), AuxiliaryILISynset(ili))
        case _ => Nil
      }
    } else {
      r.target match {
        case iliRef(ili) => Seq(AuxiliaryILISynset(ili))
        case _ => Nil
      }
    }


  def toGWNEntry(entry : (String, Entry)) = entry match {
    case (id, Entry(lemma, examples, status, senses, _)) =>
      val grouped = senses.groupBy(_.pos).values
      if(id == "318") println(grouped)
      for {
           entryGroup <- grouped
      } yield  toGWNEntry2(id, lemma, status, entryGroup)
  }

  def toGWNEntry2(id : String, _lemma : String, 
    status : String, senses : List[Sense]) = {
      val lemma = _lemma.replaceAll("^\\*", "")
      val pos = senses(0).pos
        s"""    <LexicalEntry id=\"${lemmaEscape(lemma)}-$pos\" note=\"$status\">
      <Lemma writtenForm=\"$lemma\" partOfSpeech=\"$pos\"/>
${senses.map(toGWNSense(_, id, lemma)).mkString("\n")}
    </LexicalEntry>"""
  }

  def toGWNSense(sense : Sense, id : String, lemma : String) = 
    if(sense.synonym == "") {
      s"""      <Sense id="${lemmaEscape(lemma)}-$id-${sense.id}" synset="${id}-${sense.id}">
${sense.relations.filter(x => senseRelations contains x.`type`).map(senseRelToGWN(_)).mkString("\n")}
      </Sense>"""
    } else {
      val target = sense.synonym match {
        case iliRef(ili) => ili
        case wn31Ref(id) => "wn31:" + id
        case synsetRef(id, idx) => id + "-" + idx
        case failure =>
          System.err.println(failure)
          "ERR"
      }
      s"""      <Sense id="${lemmaEscape(lemma)}-$id-${sense.id}" synset="$target"/>"""
    }

  def senseRelToGWN(r : Relation) =  {
    val target = r.target match {
      case iliSenseRef(sense, ili) => lemmaEscape(sense) + "-" + ili
      case senseRef(sense, id, senseIdx) => lemmaEscape(sense) + "-" + id + "-" + senseIdx
      case failure =>
        System.err.println(failure)
        "ERR"
    }
    if(r.`type` != "loanword") 
      s"""        <SenseRelation relType="${r.`type`}" target="$target"/>"""
    else
      s"""        <SenseRelation relType="other" dc:type="${r.`type`}" target="$target"/>"""
      
  }

  def toGWNSynset(e : (String, Entry)) = e match {
    case (id, entry) => entry.senses.filter(_.synonym == "").map{toGWNSynset2(_,id,entry.lemma)}
  }

  def toGWNSynset2(sense : Sense, id : String, lemma : String) = 
    s"""    <Synset id=\"$id-${sense.id}" ili="in" partOfSpeech="${sense.pos}"> <!-- $lemma -->
      <Definition>${sense.definition}</Definition>
${sense.relations.filter(x => !(senseRelations contains x.`type`)).map(synRelToGWN(_)).mkString("\n")}
    </Synset>"""

  def synRelToGWN(r : Relation) = {
    val target = r.target match {
      case iliRef(ili) => ili
      case wn31Ref(id) => "wn31:" + id
      case synsetRef(id, idx) => id + "-" + idx
      case failure =>
        System.err.println(failure)
        "ERR"
    }
    if(r.`type` != "emotion" && r.`type` != "pejorative")
      s"""      <SynsetRelation relType=\"${r.`type`}\" target="$target"/>"""
    else
      s"""      <SynsetRelation relType="other" dc:type="${r.`type`}" target="$target"/>"""
  }

  def main(args : Array[String]) {
    val out = new java.io.PrintWriter("colloqwn.xml")
    out.println(
      toGWN(
        FileDataStore.list.map(id => (id.replaceAll("^\\-+",""), FileDataStore.get(id))).filter({
          case (id, entry) => 
            (entry.status == "general" || entry.status == "novel" || entry.status == "vulgar") &&
            !entry.senses.exists(_.pos == "x") &&
            !entry.senses.exists(_.synonym != "")
        }).take(98)
      )
    )
    out.flush
    out.close
  }
}
