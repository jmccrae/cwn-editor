package org.insightcentre.cwneditor

import java.io.PrintWriter
import org.insightcentre.cwneditor.db.DB

object ToGWN {
  def toGWN(db : DB, out : PrintWriter) {
    out.println(
s"""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE LexicalResource SYSTEM "http://globalwordnet.github.io/schemas/WN-LMF-1.0.dtd">
<LexicalResource xmlns:dc="http://purl.org/dc/elements/1.1/">
  <Lexicon id="colloqwn"
           label="Colloquial WordNet derived from Twitter"
           language="en" 
           email="john@mccr.ae"
           license="https://creativecommons.org/licenses/by/4.0/"
           version="1.1-SNAPSHOT"
           citation="Towards a Crowd-Sourced WordNet for Colloquial English. John P. McCrae, Ian Wood and Amanda Hicks, Proceedings of the 9th Global WordNet Conference, (2018)."
           url="http://colloqwn.linguistic-lod.org/">""")
    val targets = printEntries(db, out)
    printAuxEntries(targets, out)
    printSynsets(db, out, targets)
out.println("""  </Lexicon>
</LexicalResource>""")
  }

  private def xmlId(s : String) = s.replaceAll(" ", "_")
    .replaceAll("'", "_")
    .replaceAll("/", "_")

  private def synsets(db : DB, entry : Entry) : Seq[Synset]= 
    entry.senses.flatMap(sense => db.getSynset(sense.synset))


  private def printEntries(db : DB, out : PrintWriter) = {
    var targets = collection.mutable.Set[(String, String)]()
    var nonTargets = collection.mutable.Set[(String, String)]()
    for(id <- db.list) {
      for(entry <- db.get(id)) {
        val ss = synsets(db, entry)
        for(sense <- entry.senses if ss.exists(s => s.id == sense.synset)) {
          nonTargets += ((entry.lemma, sense.synset))
        }
      }
    }
    for(id <- db.list) {
      for(entry <- db.get(id)) {
        val ss = synsets(db, entry)
        val poses = ss.map(_.pos).toSet
        for(_pos <- poses) {
          val pos = if(_pos.length == 3) _pos(1) else _pos
        out.print(s"""
      <LexicalEntry id="${xmlId(entry.lemma)}-${pos}" note="${entry.status}">
        <Lemma writtenForm="${entry.lemma}" partOfSpeech="$pos"/>""")
        for(sense <- entry.senses if ss.exists(s => s.id == sense.synset && s.pos == pos)) {
          targets += (("", sense.synset))
          out.print(s"""
          <Sense id="${xmlId(entry.lemma)}-${sense.synset}" synset="${sense.synset}">""")
          for(rel <- sense.relations if !rel.trgSynset.startsWith("p")) {
          out.print(s"""
            <SenseRelation relType="${relType(rel.`type`)}" target="${xmlId(rel.trgWord)}-${rel.trgSynset}"/>""")
          targets += ((rel.trgWord, rel.trgSynset))
          }
          out.print("""
          </Sense>""")
        }
        out.print(s"""
      </LexicalEntry>""")
        }
      }
    }
    targets --= nonTargets
    targets.toSet
  }

  private def printAuxEntries(targets : Set[(String, String)], out : PrintWriter) {
    for {
      e <- targets
            .filter(s => (s._2.startsWith("i") || s._2.startsWith("wn31")) && s._1 != "")
            .groupBy(_._1)
            .values
    } {
      val lemma = e.toSeq.head._1
      out.print(s"""
    <LexicalEntry id="${xmlId(lemma)}" note="aux">
      <Lemma writtenForm="$lemma" partOfSpeech="u"/>""")
      for(e2 <- e) {
        out.print(s"""
        <Sense id="${xmlId(lemma)}-${e2._2}" synset="${e2._2}"/>""")
      }
      out.print(s"""
    </LexicalEntry>""")
    }
  }

  private def relType(r : String) = r match {
    case "emotion" => "other\" dc:type=\"emotion"
    case "loanword" => "other\" dc:type=\"loanword"
    case "pejorative" => "other\" dc:type=\"pejorative"
    case r => r
  }

  private def printSynsets(db : DB, out : PrintWriter, targets : Set[(String, String)]) {
    var targets2 = collection.mutable.Set[String]()
    val targetSet = targets.map(_._2).toSet
    for(t <- targetSet) {
      if(t.startsWith("c")) {
        for(synset <- db.getSynset(t)) {
          out.print(s"""
    <Synset id="${synset.id}" ili="in" partOfSpeech="${synset.pos}">
      <Definition>${synset.definition}</Definition>""")
          for(rel <- synset.relations) {
            out.print(s"""
      <SynsetRelation relType="${relType(rel.`type`)}" target="${rel.trgSynset}"/>""")
            
            if(!targetSet.contains(rel.trgSynset)) {
              targets2.add(rel.trgSynset)
            }
          }
          out.print("""
    </Synset>""")
        }
      } else {
        out.print(s"""
    <Synset id="${t}" ili="${if(t.startsWith("i")) { t } else { "" }}" note="aux"/>""")
      }
    }
    for(t <- targets2) {
        out.print(s"""
    <Synset id="${t}" ili="${if(t.startsWith("i")) { t } else { "" }}" note="aux"/>""")
    }


  }
}
