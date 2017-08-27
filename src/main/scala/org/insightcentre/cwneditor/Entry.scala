package org.insightcentre.cwneditor

case class Entry(val lemma : String, 
    val confidence : String,
    val examples : List[Example],
    status : String, senses : List[Sense], editorId : String) {
  def addExample(example : String) = Entry(lemma, confidence,
    Example(example) :: examples, status, senses, editorId)

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
  val DEFN_REGEX = "(.*): (.*) <(.*)>".r
}

case class Example(text : String)

case class Sense(
    pos : String,
    definition : String,
    synonym : String,
    relations : List[Relation],
    id : Int) {
  def isSyn = synonym != ""
  def synWord = synonym match {
    case Entrys.DEFN_REGEX(w, _, _) => w
    case x => x
  }
  def synDefn = synonym match {
    case Entrys.DEFN_REGEX(_, d, _) => d
    case _ => ""
  }
  def synLink = synonym match {
    case Entrys.DEFN_REGEX(_, _, l) => {
      if(l.startsWith("i")) {
        "http://ili.globalwordnet.org/ili/" + l
      } else {
        "http://colloqwn.linguistic-lod.org/show/" + l
      }
    }
    case _ => ""
  }
  def partOfSpeech = pos match {
    case "n" => "noun"
    case "v" => "verb"
    case "a" => "adjective"
    case "r" => "adverb"
    case "x" => "other"
  }
}

case class Relation(`type` : String, target : String, relNo : Int) {
  def trgWord = target match {
    case Entrys.DEFN_REGEX(w, _, _) => w
    case x => x
  }
  def trgDefn = target match {
    case Entrys.DEFN_REGEX(_, d, _) => d
    case _ => ""
  }
  def trgLink = target match {
    case Entrys.DEFN_REGEX(_, _, l) => {
      if(l.startsWith("i")) {
        "http://ili.globalwordnet.org/ili/" + l
      } else {
        "http://colloqwn.linguistic-lod.org/show/" + l
      }
    }
    case _ => ""
  }
}
