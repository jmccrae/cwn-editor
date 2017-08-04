import java.io.File
import com.fasterxml.jackson.databind.ObjectMapper

object ProcessLittleFireHose {
  def main(args : Array[String]) {
    val dir = args(0)
    val prefix = args(1)
    val mapper = new ObjectMapper()
    for(f <- new File(dir).listFiles if f.getName().startsWith(prefix)) {
      for(line <- io.Source.fromFile(f).getLines) {
        if(line != "") {
          val data = mapper.readTree(line)
          data.get("lang") match {
            case null =>
            case node if node.textValue() == "en" =>
              println(Option(data.get("text")).getOrElse(""))
          }
        }
      }
    }
  }
}
