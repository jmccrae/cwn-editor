import java.io.{File, PrintWriter, FileInputStream, FileOutputStream}
import com.fasterxml.jackson.databind.ObjectMapper
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

object ProcessLittleFireHose {
  case class Config(dir : File = new File("."), 
    prefix : String = "littleFireHose.json.gz", 
    out : File = new File("tweets.txt.gz"))

  def main(args : Array[String]) {
    val parser = new scopt.OptionParser[Config]("Process Little Firehose") {
      head("ProcessLittleFireHose", "0.1")

      opt[File]('d', "dir")
        .action((x,c) => c.copy(dir=x))
        .text("The directory containing the files")

      opt[String]('p', "prefix")
        .action((x,c) => c.copy(prefix=x))
        .text("The prefix that files start with")

      opt[File]('o', "out")
        .action((x,c) => c.copy(out=x))
        .text("Where to write the output to")
    }

    parser.parse(args, Config()) match {
      case Some(config) =>
        val out = new java.io.PrintWriter(
          new GZIPOutputStream(
            new FileOutputStream(config.out)))
        val mapper = new ObjectMapper()
        for(f <- config.dir.listFiles if f.getName().startsWith(config.prefix)) {
          println(f.getName())
          for(line <- io.Source.fromInputStream(
               new GZIPInputStream(
                 new FileInputStream(f))).getLines) {
            if(line startsWith "{") {
              val data = mapper.readTree(line)
              data.get("lang") match {
                case null =>
                case node if node.textValue() == "en" =>
                  out.println("%s,%s" format (
                    Option(data.get("created_at")).getOrElse(""),
                    Option(data.get("id")).getOrElse(""),
                    Option(data.get("text")).getOrElse("")))
                case _ =>
              }
            }
          }
        }
      case None =>
        parser.showUsageAsError()
        
    }
  }
}
