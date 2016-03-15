import model.Dictionary
import scala.util.control.Breaks._

case class Token(token: Seq[(String, String)]) {

  override def toString() = token.map {
    case (key, value) => key + " -> " + value
  }.mkString("\n")

}

object Token {

  def apply(text: String): Token = Token(parseScaner(scanner(text, ' ')))

  private var token: Seq[(String, String)] = Seq()

  private def insertAndTakeNext(text: String, name: String, value: String, other: Option[Char] = None) = {
    token = token :+(name, value)
    scanner(text.tail, other.getOrElse(value(0)))
  }

  private def insertAndCheckNext(text: String, name: String, value: Char, other: Option[Char] = None) = {
    token = token :+(name, value.toString)
    scanner(text.tail, other.getOrElse(value))
  }

  private def replaceAndCheckNext(text: String, value: Char, insertOther: Option[Char] = None) = {
    token = token.dropRight(1) :+(token.last._1, token.last._2 + value)
    scanner(text.tail, insertOther.getOrElse(value))
  }

  private def insertAndCheckNext(text: String, name: String) = {
    val textToInsert = name match {
      case "Quotes" => stringText(text)
      case "MultilineComment" => multilineComment(text)
      case "SingleLineComment" => singleLineComment(text)
    }
    token = token :+(name, textToInsert)
    scanner(text.substring(textToInsert.length), '_')
  }

  private def stringText(text: String) = "\"" + text.tail.takeWhile(_ != '\"') + "\""

  private def singleLineComment(text: String) = {
    val result = text.takeWhile(_ != '\n') + "\n"
    if(result.length>text.length) text
    else result
  }

  private def multilineComment(text: String) : String = {
    var comment = "/*"
    for (i <- 2 to text.length()) {
      if (text(i) == '*' && text(i + 1) == '/') return comment + "*/"
      comment += text(i)
    }
    comment + "*/"
  }

  private def parseScaner(tokenToEdit: Seq[(String, String)]): Seq[(String, String)] = tokenToEdit map {
      case (_, value) if Dictionary.getSk.contains(value) => ("Sk", value)
      case (_, value) if Dictionary.getTypeOfVariable.contains(value) => ("Type", value)
      case (_, value) if Dictionary.getReservedWord.contains(value) => ("Reserved", value)
      case (key, value) if key.equals("Ident") && value(0).isDigit => ("Undefined", value)
      case other => other

  }


  private def scanner(text: String, lastChar: Char): Seq[(String, String)] = {
    if (text.length() > 0)
      text.head match {

        case quotes if quotes == '\"' => {
          token = token :+("Quotes", quotes.toString)
          breakable {
          for(i <-1 until text.size){
            if(text(i) == quotes){
              token = token.dropRight(1) :+(token.last._1, token.last._2 + quotes)

              scanner(text.substring(i+1), ' ')
              break
            }
            else{
              token = token.dropRight(1) :+(token.last._1, token.last._2 + text(i))
            }
          }}

          token
        }

        case ident if ident.isLetter => {
          token = token :+("Ident", ident.toString)
          breakable {
            for (i <- 1 until text.size) {
              if (text(i).isDigit || text(i).isLetter) {
                token = token.dropRight(1) :+(token.last._1, token.last._2 + text(i))
              }
              else {
                scanner(text.substring(i), text(i))
                break
              }
            }
          }
          token
        }

        case number if number.isDigit => {
          token = token :+("Number", number.toString)
          var isDot = false
          breakable {
            for (i <- 1 until text.size) {
              if (text(i).isDigit) {
                token = token.dropRight(1) :+(token.last._1, token.last._2 + text(i))
              } else if(text(i)=='.' && isDot==false) {
                isDot= true;
                if(text.substring(i).length>0 && text(i+1).isDigit)
                  token = token.dropRight(1) :+("Double", token.last._2 + text(i))
                else{
                  scanner(text.substring(i), text(i))
                  break
                }
              }
              else {
                scanner(text.substring(i), text(i))
                break
              }
            }
          }
          token
        }

        case space if space ==' ' => {
          insertAndCheckNext(text, "Space", space)
        }
        case div if div.equals('/') =>
          if (text.length() > 1 && text(1) == '*') insertAndCheckNext(text, "MultilineComment")
          else if (text.length() > 1 && text(1) == '/') insertAndCheckNext(text, "SingleLineComment")
          else if (text.length() > 1 && text(1) == '=') insertAndTakeNext(text.tail, "TwoMathChar", "/=", Some(div))
          else insertAndCheckNext(text, "Div", div)

        case multiply if multiply.equals('*') =>
          if (text.length() > 1 && text(1) == '*') insertAndTakeNext(text.tail, "Power", "**", Some(multiply))
          else if (text.length() > 1 && text(1) == '=') insertAndTakeNext(text.tail, "TwoMathChar", "*=", Some(multiply))
          else insertAndCheckNext(text, "Multiply", multiply)

        case assign if assign.equals('=') =>
          if (text.length() > 1 && text(1) == '=') insertAndTakeNext(text.tail, "Equals", "==", Some(assign))
          else insertAndCheckNext(text, "Assign", assign)


        case plus if plus.equals('+') =>
          if (text.length() > 1 && text(1) == '=') insertAndTakeNext(text.tail, "TwoMathChar", "+=", Some(plus))
          else insertAndCheckNext(text, "Plus", plus)

        case more if more.equals('>') =>
          if (text.length() > 1 && text(1) == '=') insertAndTakeNext(text.tail, "MoreOrEqual", ">=", Some(more))
          else insertAndCheckNext(text, "More", more)

        case less if less.equals('<') =>
          if (text.length() > 1 && text(1) == '=') insertAndTakeNext(text.tail, "LessOrEqual", ">=", Some(less))
          else insertAndCheckNext(text, "Less", less)

        case notEqual if notEqual.equals('!') =>
          if (text.length() > 1 && text(1) == '=') insertAndTakeNext(text.tail, "NotEqual", "!=", Some(notEqual))
          else insertAndCheckNext(text, "Negation", notEqual)

        case semicolon if semicolon.equals(';') => insertAndCheckNext(text, "Semicolon", semicolon)

        case coma if coma.equals(',') => insertAndCheckNext(text, "Coma", coma)

        case minus if minus.equals('-') =>
          if (text.length() > 1 && text(1) == '=') insertAndTakeNext(text.tail, "TwoMathChar", "-=", Some(minus))
          else insertAndCheckNext(text, "Minus", minus)

        case dot if dot.equals('.') => insertAndCheckNext(text, "Dot", dot)

        case bracket if Dictionary.getBracket.contains(bracket)  => insertAndCheckNext(text, "Bracket", bracket)

        case newLine if newLine=='\n'  => insertAndCheckNext(text, "NewLine", '_')

        case newLine if newLine=='\t'  => insertAndCheckNext(text, "Tabular", '_')

        case notDefined => insertAndCheckNext(text, "notDefined", notDefined, Some('_'))
      }
    else{
      token.map {
      case (_, value) if Dictionary.getSk.contains(value) => ("Sk", value)
      case (_, value) if Dictionary.getTypeOfVariable.contains(value) => ("Type", value)
      case (_, value) if Dictionary.getReservedWord.contains(value) => ("Reserved", value)
      case (key, value) if key.equals("Ident") && value(0).isDigit => ("Undefined", value)
      case other => other
    }
    }
  }
}