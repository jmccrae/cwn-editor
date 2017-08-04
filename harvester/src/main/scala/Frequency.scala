import java.util.regex.Pattern
import java.io.File
import scala.util.{Try,Success,Failure}

object Frequency {
  val ENGLISH_STOPWORDS = Set("a", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "aren't", "as", "at", "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "can't", "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't", "doing", "don't", "down", "during", "each", "few", "for", "from", "further", "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd", "he'll", "he's", "her", "here", "here's", "hers", "herself", "him", "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if", "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me", "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off", "on", "once", "only", "or", "other", "ought", "our", "ours	ourselves", "out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's", "should", "shouldn't", "so", "some", "such", "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then", "there", "there's", "these", "they", "they'd", "they'll", "they're", "they've", "this", "those", "through", "to", "too", "under", "until", "up", "very", "was", "wasn't", "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what", "what's", "when", "when's", "where", "where's", "which", "while", "who", "who's", "whom", "why", "why's", "with", "won't", "would", "wouldn't", "you", "you'd", "you'll", "you're", "you've", "your", "yours", "yourself", "yourselves")
  val pattern1 = Pattern.compile("(\\.\\.\\.+|[\\p{Po}\\p{Ps}\\p{Pe}\\p{Pi}\\p{Pf}\u2013\u2014\u2015&&[^'\\.]]|(?<!(\\.|\\.\\p{L}))\\.(?=[\\p{Z}\\p{Pf}\\p{Pe}]|\\Z)|(?<!\\p{L})'(?!\\p{L}))");
  val pattern2 = Pattern.compile("\\p{C}|^\\p{Z}+|\\p{Z}+$");

  def tokenize(s : String) = {
    val s1 = pattern1.matcher(s).replaceAll(" $1 ");
    val s2 = pattern2.matcher(s1).replaceAll("");                               
    s2.split("\\p{Z}+");
  }

  def context(tokens : Array[String], i : Int, j : Int) = {
    tokens.slice(math.max(0, i - 8), math.min(tokens.length, j + 8)).mkString(" ")
  }

  case class Config(
    termsFile : File = new File("terms.txt"),
    stopwordsFile : Option[File] = None,
    unigramFile : File = new File("count_1w.txt"),
    bigramFile : File = new File("count_2w.txt"),
    corpusFile : File = new File("corpus.txt"),
    wordnetFile : File = new File("wn31.xml")
  ) {

    def validateTerms : Option[String] =
      Option(termsFile.exists).collect({
        case false => "Terms file does not exist" 
      })

    def validateStopwords : Option[String] =
      stopwordsFile.flatMap(s => Option(s.exists).collect({
         case false => "Stopwords file does not exist"
      }))

    def validateUnigrams : Option[String] =
      Option(unigramFile.exists).collect({
          case false => "Unigram file does not exist"
      })

    def validateBigrams : Option[String] =
      Option(bigramFile.exists).collect({
          case false => "Bigram file does not exist"
      })

    def validateCorpus = 
      Option(corpusFile.exists).collect({
          case false => "Corpus file does not exist"
      })

    def validateWordnet =
      Option(wordnetFile.exists).collect({
        case false => "WordNet file does not exist"
      })

    def validate : Seq[String] = 
      (validateTerms ++ validateStopwords ++ validateUnigrams ++ 
        validateBigrams ++ validateCorpus ++ validateWordnet).toSeq
  }


  def main(args : Array[String]) {
    val parser = new scopt.OptionParser[Config]("Frequency") {
      head("Frequency","0.1")

      opt[File]('t',"terms")
        .action( (x,c) => c.copy(termsFile=x))
        .text("The list of terms to accept (e.g., from HarvestUrbanDic)")

      opt[File]("stopwords")
        .action((x, c) => c.copy(stopwordsFile=Some(x)))
        .text("The stopword list file (optional)")

      opt[File]("unigrams")
        .action((x, c) => c.copy(unigramFile=x))
        .text("The unigram file e.g., from http://norvig.com/ngrams/count_1w.txt")

      opt[File]("bigrams")
        .action((x, c) => c.copy(bigramFile=x))
        .text("The bigram file e.g., from http://norvig.com/ngrams/count_2w.txt")

      opt[File]('c', "corpus")
        .action((x, c) => c.copy(corpusFile=x))
        .text("The corpus file")

      opt[File]("wordnet")
        .action((x, c) => c.copy(wordnetFile=x))
        .text("The wordnet file, e.g., http://john.mccr.ae/wn31.xml")

    }
    parser.parse(args, Config()) match {
      case Some(config) =>
        config.validate match {
          case msgs if !msgs.isEmpty => 
            msgs.foreach(System.err.println)
            parser.showUsageAsError()
          case _ =>
            val words = io.Source.fromFile(config.termsFile).getLines().map({ line =>
              if(line.startsWith("* ")) {
                line.drop(2).toLowerCase()
              } else {
                line.toLowerCase()
              }


            }).toSet

            val stopwords = Try(io.Source.fromFile(config.stopwordsFile.get).getLines.toSet).getOrElse(ENGLISH_STOPWORDS)

            val wordnet_words = {
              val wordnet = xml.XML.loadFile(config.wordnetFile)

              (wordnet \\ "Lemma").map(x => (x \ "@writtenForm").text.toLowerCase()).toSet
            }

            val unigram_counts : Map[String, Double] = io.Source.fromFile(config.unigramFile).getLines().map({
              line => 
                val x = line.split("\t")
                x(0) -> x(1).toDouble
            }).toMap

            val bigram_counts = io.Source.fromFile(config.bigramFile).getLines().map({
              line => 
                val x = line.split("\t")
                x(0) -> x(1).toDouble
            }).toMap



                                                                                            
            val freqs = collection.mutable.Map[String, Double]()
            val contexts = collection.mutable.Map[String, Seq[String]]()

            def falsePostive(word : String) = 
              if(wordnet_words.contains(word) || stopwords.contains(word)) {
                true
              } else if(word.endsWith("ing") && wordnet_words.contains(word.dropRight(3))) {
                true
              } else if(word.endsWith("s") && wordnet_words.contains(word.dropRight(1))) {
                true
              } else if(word.startsWith("i ") && wordnet_words.contains(word.drop(2))) {
                true
              } else if(word.startsWith("a ") && wordnet_words.contains(word.drop(2))) {
                true
              } else if(word.startsWith("the ") && wordnet_words.contains(word.drop(4))) {
                true
              } else if(word.startsWith("to ") && wordnet_words.contains(word.drop(3))) {
                true
               } else {
                false
              }

            //val corpusFile = "reddit.txt"
            def corpusFile = { 
              if(config.corpusFile.getName().endsWith(".gz")) {
                io.Source.fromInputStream(
                  new java.util.zip.GZIPInputStream(
                   new java.io.FileInputStream(config.corpusFile))).getLines 
              } else {
                io.Source.fromFile(config.corpusFile).getLines
              }
            }

            
            val baseFreq = 1e6

            //val N : Int = io.Source.fromFile(corpusFile).getLines.map(line => tokenize(line.toLowerCase()).size).sum
            val N : Int = corpusFile.map(line => tokenize(line.toLowerCase()).size).sum
            val N2 = unigram_counts.values.sum

            for(line <- corpusFile) {
              val tokens = tokenize(line.toLowerCase().replaceAll("@\\S+",""))
              var contexted = collection.mutable.Set[String]()
              for(i <- 0 until tokens.size) {
                for(j <- (i+1) to math.min(i + 3, tokens.size)) {
                  val phrase = tokens.slice(i, j).mkString(" ")
                  if(words.contains(phrase)) {
                    if(j - i == 1) {
                      val f : Double = freqs.getOrElse(phrase, 0.0)
                      val u : Double = unigram_counts.getOrElse(phrase, baseFreq) / N2 * N
                      freqs.put(phrase, f + 1.0 / u)
                    } else if(j - i == 2) {
                      val f : Double = freqs.getOrElse(phrase, 0.0)
                      val b : Double = bigram_counts.getOrElse(phrase, baseFreq) / N2 * N
                      freqs.put(phrase, f + 1.0 / b)
                    } else {
                      val f : Double = freqs.getOrElse(phrase, 0.0)
                      val b : Double = bigram_counts.getOrElse(tokens.slice(i, i + 2).mkString(" "), baseFreq) / N2 * N
                      freqs.put(phrase, f + 1.0 / b)
                    }
                    if(!(contexted contains phrase)) {
                      contexts.put(phrase, context(tokens, i, j) +: contexts.getOrElse(phrase, Seq()))
                      contexted.add(phrase)
                    }
                  }
                }
              }
            }

            for((phrase, freq) <- freqs if freq > 5.0) {
              for(context <- contexts(phrase).take(10)) {
                println("%f\t%s%s\t\"%s\"" format (freq, if(falsePostive(phrase)) { "*" } else { "" }, phrase, context.replaceAll("\\\"","\\\\\"")))
              }
            }
        }
      case None =>
        parser.showUsageAsError()
    }
  }
}
