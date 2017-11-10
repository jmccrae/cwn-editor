package org.insightcentre.cwneditor 
import scala.xml._

import db.DB

object FromGWN {
  val LEMMA_ILI = "(.*)-(i\\d+)".r
  val LEMMA_CWN = "(.*)-(\\d+\\-\\d)".r
  val LEMMA_RED = "(.*)-(\\d+reddit\\d+-\\d)".r
  val LEMMA_WN31 = "wn31-(.*)-(.)#(\\d+)".r

  def makeSenseRelations(sense : Node,
    cwnSynsets : Map[String, Synset]) : List[Relation] = {
    (sense \ "SenseRelation").zipWithIndex.map({ 
      case (sr, idx) =>
        (sr \ "@target").text match {
          case "Instagram-06-1" =>
            Relation(srType(sr),
              "Instagram", cwnSynsets("Instagram").id)
          case "wn31-malware-n-06600315" =>
            Relation(srType(sr),
              "malware", cwnSynsets("wn31-06600315-n").id)
          case "tweet-0-1" =>
            Relation(srType(sr),
              "tweet", cwnSynsets("colloqwn-00000-1").id)
          case LEMMA_ILI(lemma, ili) => 
            Relation(srType(sr),
              lemma.replaceAll("_", " "), ili)
          case LEMMA_CWN(lemma, cwn) =>
            Relation(srType(sr),
              lemma.replaceAll("_", " "), cwnSynsets("colloqwn-" + cwn).id)
          case LEMMA_RED(lemma, cwn) =>
            Relation(srType(sr),
              lemma.replaceAll("_", " "), cwnSynsets("colloqwn-" + cwn).id)
          case LEMMA_WN31(lemma, pos, wn) =>
            Relation(srType(sr),
              lemma.replaceAll("_", " "), cwnSynsets(s"wn31-$wn-$pos").id)
        }
    }).toList
  }

  def srType(sr : Node) = (sr \ "@relType").text match {
    case "other" => (sr \ "@{http://purl.org/dc/elements/1.1/}type").text
    case notOther => notOther
  }

  def fail2aux(s : String) = s match {
    case null => "aux"
    case "" => "aux"
    case other => other
  }

  def consolidateEntries(es : NodeSeq, corrections : Seq[Entry],
    synsets : Map[String, Synset], examples : Map[String, List[Example]]) : Seq[Entry] = {
    val es1 : Seq[Entry] = es.map({ e =>
      val lemma = (e \ "Lemma" \ "@writtenForm").text
      Entry(
        lemma=lemma,
        confidence="vstrong",
        examples=examples.getOrElse(lemma, Nil),
        status=fail2aux((e \ "@note").text),
        senses=(e \ "Sense").flatMap({ s =>
          synsets.get((s \ "@synset").text).map({ synset =>
              Sense(
                relations=makeSenseRelations(s, synsets),
                synset=synset.id)
            })
        }).toList)
    }) ++ corrections
    val es2 : Map[String, Seq[Entry]] = es1.groupBy(_.lemma)
    val es3 : Map[String, Entry] = es2.mapValues({
      case Seq() => throw new RuntimeException("Unreachable")
      case Seq(e) => e
      case vs : Seq[Entry] => vs.reduce[Entry]({
        case (x,y) => 
          val status = if(x.status == "aux") {
            y.status
          } else if(y.status == "aux") {
            x.status
          } else if(x.status == y.status) {
            x.status
          } else if(x.lemma == "dab") {
            "novel"
          } else {
            throw new RuntimeException(x.toString + " vs " + y.toString)
          }
          Entry(
            lemma=x.lemma,
            confidence=x.confidence,
            examples=x.examples ++ y.examples,
            status=status,
            senses=x.senses ++ y.senses)
      })
    })
    es3.values.toSeq
  }

  def main(args : Array[String]) {
    val xmlFile = if(args.length < 1) {
      "colloqwn-1.0.xml"
    } else {
      args(0)
    }

    val pwnFile = if(args.length < 2) {
      "wn31.xml"
    } else {
      args(1)
    }

    val csvFile = if(args.length < 3) {
      "colloqwn-corrections.csv"
    } else {
      args(2)
    }

    val examplesFile = if(args.length < 4) {
      "colloqwn-examples.tsv"
    } else {
      args(3)
    }

    val user = if(args.length < 5) {
      "jmccrae"
    } else {
      args(4)
    }

    val examples = io.Source.fromFile(examplesFile, "UTF-8").getLines.map({ line =>
      val e = line.split("\t")
      if(e.size == 1) {
        e(0) -> Nil
      } else {
        e(0) -> e(1).split(";;;").toList.map(Example)
      }
    }).toMap

    val gwn = XML.loadFile(xmlFile)

    var cwns = 0

    val cwnSynsets = (gwn \\ "Synset")
    .filter(s => (s \ "@note").text != "aux")
    .map({ s =>
      cwns += 1
      val id = s"c$cwns"
      (s \ "@id").text -> Synset(
        id=id,
        pos=(s \ "@partOfSpeech").text,
        definition=(s \ "Definition").text,
        relations=(s \ "SynsetRelation").map(sr =>
            SynsetRelation(srType(sr), (sr \ "@target").text)).toList)
    }).toMap

    val pwn = XML.loadFile(pwnFile)

    val entries = (gwn \\ "LexicalEntry").filter(e =>
      (e \ "@note").text != "aux" ) ++ (pwn \\ "LexicalEntry")

    var nonILIpwn = 0

    val pwnSynsets = (pwn \\ "Synset").map({ s => 
      val ili = (s \ "@ili").text
      if(ili != "in") {
        (s \ "@id").text -> Synset(
          id=(s \ "@ili").text,
          pos=(s \ "@partOfSpeech").text,
          definition=(s \ "Definition").text,
          relations=(s \ "SynsetRelation").map(sr =>
              SynsetRelation(srType(sr), (sr \ "@target").text)).toList)
      } else {
        nonILIpwn += 1
        val id = s"p$nonILIpwn"
        (s \ "@id").text -> Synset(id=id, 
          pos=(s \ "@partOfSpeech").text,
          definition=(s \ "Definition").text,
          relations=(s \ "SynsetRelation").map(sr =>
              SynsetRelation(srType(sr), (sr \ "@target").text)).toList)
      }
    })

    val corrections = io.Source.fromFile(csvFile).getLines.map({ line =>
      val e = line.split("\t")
      Entry(
        lemma=e(1),
        confidence="vstrong",
        examples=examples.getOrElse(e(1), Nil),
        status=e(0),
        senses=if(e.length == 3) { List(Sense(Nil,e(2)))} else { Nil })
    }).toSeq



    val synsets2 : Map[String, Synset] = (cwnSynsets ++ pwnSynsets)
    val synsets = synsets2.mapValues({ s =>
      s.copy(relations=s.relations.map({sr =>
        if(sr.trgSynset.matches("[icp]\\d+")) {
          sr
        } else {
          sr.copy(trgSynset=synsets2(sr.trgSynset).id)
        }
      }))
    })

    val entry2synset = collection.mutable.Map[Int,Seq[String]]();
    val synset2ids = collection.mutable.Map[String,Int]();

    val db = new DB(new java.io.File("cwn.db"))

    val loader = db.init
    try {

      for(entry <- consolidateEntries(entries, corrections, synsets, examples)) {
        val dbId = loader.insertEntry(entry, user)
        entry2synset.put(dbId, entry.senses.
          filter(_.synset.matches("[cip]\\d+")).
          map(_.synset))
      }

      for((_, data) <- synsets) {
        val dbId = loader.insertSynset(data, user)
        synset2ids.put(data.id, dbId)
      }

      for((eid, ssids) <- entry2synset) {
        for(ssid <- ssids) {
          loader.insertEntrySynset(eid, synset2ids(ssid))
        }
      }

      if(new java.io.File("users.csv").exists) {
        for(line <- io.Source.fromFile("users.csv").getLines) {
          line.split(",") match {
            case Array(i,u,h,e,r) if i != "id" =>
              loader.insertUser(u,h,e,r)
            case Array(i,u,h,e) if i != "id" =>
              loader.insertUser(u,h,e,"jmccrae")
            case _ =>
              println("Skipped user line: " + line)
          }
        }
      }


      loadQueue(loader)
    } finally {
      loader.close
    }

  }

  def loadQueue(loader : db.DB#DBLoader) {
      io.Source.fromInputStream(
        new java.util.zip.GZIPInputStream(
          new java.io.FileInputStream("queue.csv.gz")), "UTF-8").getLines.foreach({ line =>
            val e = line.split("\\|\\|\\|")
            loader.insertQueue(e(1), e(2))
          })
  }
}
