import java.net.{URL,HttpURLConnection}

object HarvestUrbanDic {
  val link = """<a (class="popular" )?href="/define.php\?term=[^"]*">([^<]*)</a>""".r

  def main(args : Array[String]) {
    val out = new java.io.PrintWriter("terms.txt")

    for(letter <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ") {
      var page = 1
      var found = true
      while(found) {
        found = false
        val req = new URL(
          "http://www.urbandictionary.com/browse.php?character=%s&page=%d" 
                format (letter, page))
                  .openConnection()
                  .asInstanceOf[HttpURLConnection]
        req.setInstanceFollowRedirects(false)
        if(req.getResponseCode() == 200) {
            for(line <- io.Source.fromInputStream(req.getInputStream()).getLines) {
              link.findAllMatchIn(line).foreach({
                m =>
                  val term = m.group(2)
                  val pop = m.group(1)
                  println(term)
                  found = true
                  if(pop != "") {
                    out.println("* " + term)
                  } else {
                    out.println(term)
                  }
              })
            }
        } else {
          found = false
        }
        println("Page: %s %d" format (letter, page))
        java.util.concurrent.TimeUnit.SECONDS.sleep(1)
        page += 1
      }
    }
    out.close
  }
}
