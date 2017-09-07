package org.insightcentre.cwneditor

object ToGWN {
//  sealed trait Auxiliary
//  case class AuxiliaryILISynset(ili : String) extends Auxiliary
//  case class AuxiliaryILISense(ili : String, lemma : String, pos : String) extends Auxiliary
//  case class AuxiliaryWN30Synset(id : String) extends Auxiliary
//
//  val iliRef = ".*<(i\\d+)>".r
//  val wn31Ref = ".*<wn31.(.*)-\\d>".r
//  val synsetRef = ".*<-+(.*)\\-(\\d)>".r
//  val iliSenseRef = "(.*?):.*<(i\\d+)>".r
//  val senseRef = "(.*?):.*<-+(.*)\\-(\\d)>".r
//  val sourcedDefinition = "(.*) \\[(.*)\\]".r
//
//  val senseRelations = Set("antonym", "derivation", "loanword")
//
//  def lemmaEscape(s : String) = s.replaceAll(" ", "_")
//
//  def toGWN(entries : Seq[(String, Entry)]) = {
//    val aux : Set[Auxiliary] = entries.flatMap(auxiliaries).toSet
//    s"""<?xml version="1.0" encoding="UTF-8"?>
//<!DOCTYPE LexicalResource SYSTEM "http://globalwordnet.github.io/schemas/WN-LMF-1.0.dtd">
//<LexicalResource xmlns:dc="http://purl.org/dc/elements/1.1/">
//  <Lexicon id="colloqwn"
//           label="Colloquial WordNet derived from Twitter"
//           language="en" 
//           email="john@mccr.ae"
//           license="https://creativecommons.org/licenses/by/4.0/"
//           version="0.1-prerelease"
//           citation=""
//           url="">
//${entries.sortBy(_._2.lemma).flatMap(toGWNEntry).mkString("\n")}
//${aux.map(toAuxEntry).mkString("\n")}
//${entries.sortBy(_._2.lemma).flatMap(toGWNSynset).mkString("\n")}
//${aux.map(toAuxSynset).mkString("\n")}
//  </Lexicon>
//</LexicalResource>"""
//  }
//
//  def toAuxEntry(aux : Auxiliary) = aux match {
//    case a : AuxiliaryILISynset => ""
//    case a : AuxiliaryWN30Synset => ""
//    case AuxiliaryILISense(ili, lemma, pos) => 
//      s"""    <LexicalEntry id="${lemmaEscape(lemma)}-$pos" note="aux">
//      <Lemma writtenForm="$lemma" partOfSpeech="$pos"/>
//      <Sense id="$lemma-$ili" synset="$ili"/>
//    </LexicalEntry>"""
//  }
//
//  def toAuxSynset(aux : Auxiliary) = aux match {
//    case AuxiliaryILISynset(ili) =>
//      s"""    <Synset id="$ili" ili="$ili" note="aux"/>"""
//    case AuxiliaryWN30Synset(id) =>
//      s"""    <Synset id="wn31:$id" ili="" note="aux"/>"""
//    case a : AuxiliaryILISense => ""
//  }
//
//  def auxiliaries(entry : (String, Entry)) : Seq[Auxiliary] = entry match {
//    case (id, Entry(_, _, _, _, senses, _)) =>
//      senses.flatMap(senseAux)
//  }
//
//  def senseAux(sense : Sense) : Seq[Auxiliary] = {
//    if(sense.synonym == "") {
//      sense.relations.flatMap(senseRelAux(_,sense.pos))
//    } else {
//      sense.synonym match {
//        case iliRef(ili) => Seq(AuxiliaryILISynset(ili))
//        case wn31Ref(id) => Seq(AuxiliaryWN30Synset(id))
//        case _ => Nil
//      }
//    }
//  }
//
//  def senseRelAux(r : Relation, pos : String) : Seq[Auxiliary] = 
//    if(senseRelations contains r.`type`) {
//      r.target match {
//        case iliSenseRef(lemma, ili) => Seq(AuxiliaryILISense(ili, lemma, pos), AuxiliaryILISynset(ili))
//        case _ => Nil
//      }
//    } else {
//      r.target match {
//        case iliRef(ili) => Seq(AuxiliaryILISynset(ili))
//        case _ => Nil
//      }
//    }
//
//
//  def toGWNEntry(entry : (String, Entry)) = entry match {
//    case (id, Entry(lemma, confidence, examples, status, senses, _)) =>
//      val grouped = senses.groupBy(_.pos).values
//      for {
//           entryGroup <- grouped
//      } yield  toGWNEntry2(id, confidence, lemma, status, entryGroup)
//  }
//
//  def mapScore(confidence : String) : String = confidence match {
//    case "vstrong" => "1.0"
//    case "strong"  => "0.9"
//    case "medium"  => "0.8"
//    case "weak"    => "0.7"
//    case ""        => "0.1"
//    case "skip"    => "0.0"
//  }
//
//  def toGWNEntry2(id : String, confidence : String, _lemma : String, 
//    status : String, senses : List[Sense]) = {
//      val lemma = _lemma.replaceAll("^\\*", "")
//      val pos = senses(0).pos
//        s"""    <LexicalEntry id=\"${lemmaEscape(lemma)}-$pos\" note=\"$status\" confidenceScore=\"${mapScore(confidence)}">
//      <Lemma writtenForm=\"$lemma\" partOfSpeech=\"$pos\"/>
//${senses.map(toGWNSense(_, id, lemma)).mkString("\n")}
//    </LexicalEntry>"""
//  }
//
//  def toGWNSense(sense : Sense, id : String, lemma : String) = 
//    if(sense.synonym == "") {
//      s"""      <Sense id="${lemmaEscape(lemma)}-$id-${sense.id}" synset="colloqwn-${id}-${sense.id}">
//${sense.relations.filter(x => senseRelations contains x.`type`).map(senseRelToGWN(_)).mkString("\n")}
//      </Sense>"""
//    } else {
//      val target = sense.synonym match {
//        case iliRef(ili) => ili
//        case wn31Ref(id) => "wn31:" + id
//        case synsetRef(id, idx) => "colloqwn-" + id + "-" + idx
//        case failure =>
//          System.err.println("Bad Target: %s in %s" format (failure, id))
//          "ERR"
//      }
//      s"""      <Sense id="${lemmaEscape(lemma)}-$id-${sense.id}" synset="$target"/>"""
//    }
//
//  def senseRelToGWN(r : Relation) =  {
//    val target = r.target match {
//      case iliSenseRef(sense, ili) => lemmaEscape(sense) + "-" + ili
//      case senseRef(sense, id, senseIdx) => lemmaEscape(sense) + "-" + id + "-" + senseIdx
//      case failure =>
//        System.err.println("Bad Sense Target: %s" format failure)
//        "ERR"
//    }
//    if(r.`type` != "loanword") 
//      s"""        <SenseRelation relType="${r.`type`}" target="$target"/>"""
//    else
//      s"""        <SenseRelation relType="other" dc:type="${r.`type`}" target="$target"/>"""
//      
//  }
//
//  def toGWNSynset(e : (String, Entry)) = e match {
//    case (id, entry) => entry.senses.filter(_.synonym == "").map{toGWNSynset2(_,id,entry.lemma)}
//  }
//
//  def toDefn(defnString : String) = defnString match {
//    case sourcedDefinition(defn, source) => """<Definition source="${source}">${defn}</Definition>"""
//    case defn => """<Definition>${defn}</Definition>"""
//  }
//
//  def toGWNSynset2(sense : Sense, id : String, lemma : String) = 
//    s"""    <Synset id=\"colloqwn-$id-${sense.id}" ili="in" partOfSpeech="${sense.pos}"> <!-- $lemma -->
//      <Definition>${sense.definition.trim()}</Definition>
//${sense.relations.filter(x => !(senseRelations contains x.`type`)).map(synRelToGWN(_)).mkString("\n")}
//    </Synset>"""
//
//  def synRelToGWN(r : Relation) = {
//    val target = r.target match {
//      case iliRef(ili) => ili
//      case wn31Ref(id) => "wn31:" + id
//      case synsetRef(id, idx) => "colloqwn-" + id + "-" + idx
//      case failure =>
//        System.err.println("Bad Syn Target: %s" format failure)
//        "ERR"
//    }
//    if(r.`type` != "emotion" && r.`type` != "pejorative")
//      s"""      <SynsetRelation relType=\"${r.`type`}\" target="$target"/>"""
//    else
//      s"""      <SynsetRelation relType="other" dc:type="${r.`type`}" target="$target"/>"""
//  }
//
//  def main(args : Array[String]) {
//    val store = new SQLDataStore(new java.io.File("cwn.db"))
//    val out = new java.io.PrintWriter("colloqwn.xml")
//    out.println(
//      toGWN(
//        store.list.map(id => (id.replaceAll("^\\-+",""), store.get(id).get)).filter({
//          case (id, entry) => 
//            (entry.status == "general" || entry.status == "novel" || entry.status == "vulgar") &&
//            !entry.senses.exists(_.synonym != "")
//        })
//      )
//    )
//    out.flush
//    out.close
//
//    val out2 = new java.io.PrintWriter("colloqwn-corrections.csv")
//    store.list.map(id => store.get(id).get).foreach({ entry =>
//      if(entry.status == "nonlex") {
//        out2.println("nonlex\t" + entry.lemma + "\t");
//      } else if(entry.status == "error") {
//        out2.println("error\t" + entry.lemma + "\t");
//      } else if(entry.status == "name") {
//        out2.println("name\t" + entry.lemma + "\t");
//      } else if(entry.status == "abbrev") {
//        for(sense <- entry.senses) {
//          out2.println("abbrev\t" + entry.lemma + "\t" + sense.definition);
//        }
//      } else if(entry.status == "misspell") {
//        for(sense <- entry.senses) {
//          out2.println("misspell\t" + entry.lemma + "\t" + sense.definition);
//        }
//      }
//    })
//    out2.flush
//    out2.close
//  }
}
