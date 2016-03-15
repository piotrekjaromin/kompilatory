import java.io.{FileWriter, BufferedWriter, File}

import scala.collection.immutable.IndexedSeq
import scala.io.Source

object Scanner extends App {
  var s1 = Source.fromFile("data.txt").mkString; //returns the file data as String

  var token: Seq[(String, String)] = Token(s1).token

  var result: Seq[String] = token map {
    case (ident, value) => ident match {
      case "Tabular" => "&nbsp;&nbsp;&nbsp;&nbsp;"
      case "Space" => "&nbsp;"
      case "NewLine" => "<br>"
      case "Quotes" => s"<font color='green'>$value</font>"
      case "SingleLineComment" => {s"<font color='#339933'>$value</font>".map(x=> if(x=='\n') "<br>" else x.toString).mkString("").toString}
      case "Reserved" => s"<font color='#ff9900'>$value</font>"
      case "Type" => s"<font color='#0033cc'>$value</font>"
      case "Ident" => s"<font color='black'>$value</font>"
      case _ => value
    }
  }
  var result2 = result.mkString("")

println(Token(s1))
  val file = new File("file2.html")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(s"<head></head><body>$result2</body>");
  bw.close()

  //println(Token("32.int 56.4*=3/=.L$e=67==L 32>=ds!=1+**\"33.4\"22*232/34/*23+3*/2-43"))
}