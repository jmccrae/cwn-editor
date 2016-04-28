package org.insightcentre.cwneditor

object FromCSV {
  def decsv(string : String) : List[String] = {
    if(string == "") {
      return Nil
    }
    var i = 0
    while(true) {
      if(i >= string.length) {
        return List(string)
      }
      string.charAt(i) match {
        case ',' =>
          return string.take(i) :: decsv(string.drop(i + 1))
        case '"' =>
          i += 1
          while(string.charAt(i) != '"') {
            if(string.charAt(i) == '\\') {
              i += 2
            } else {
              i += 1
            }
          }
        case _ =>
          i += 1
      }
    }
    Nil
  }

  def main(args : Array[String]) {
    var data = collection.mutable.Map[String,Entry]()
    var lastId = ""
    for(line <- io.Source.fromFile("/home/jmccrae/Downloads/Twitter frequencies - output.csv").getLines.drop(1)) {
      val elems = decsv(line)
      if(elems(1) == "") {
        data.put(lastId, data(lastId) addExample elems(2))
      } else {
        lastId = elems(1)
        data.put(lastId, Entry(
          elems(2), List(Example(elems(3))), 
          elems(4), List(Sense(
            elems(5),
            elems(7), elems(6), Nil, 1))))
      }
    }
    for((key, entry) <- data) {
      val f = new File("data/%s.json" format key)
      
    }
  }
}
