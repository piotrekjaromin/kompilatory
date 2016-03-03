case class Token(token: Seq[(String, String)]) {
  override def toString() = token.map {
    case (key, value) => key + " -> " + value
  }.mkString("\n")
}

object Token {

  def apply(text: String): Token = Token(scanner(text, ' '))

  private var token: Seq[(String, String)] = Seq()

  private def stringText(text: String) = "\"" + text.tail.takeWhile(_ != '\"') + "\""

  private def singleLineComment(text: String) = text.takeWhile(_ != '\n') + "\n"


  private def multilineComment(text: String) = {
    import scala.util.control._
    val loop = new Breaks;
    var comment = "/*"
    var i = 0;
    loop.breakable {
      for (i <- 2 to text.length()) {
        if (text(i) == '*' && text(i + 1) == '/') loop.break
        comment += text(i);
      }
    }
    comment += "*/"
    comment
  }

  private def scanner(text: String, lastChar: Char): Seq[(String, String)] = {

    if (text.length() > 0)
      text.head match {

        case quotes if quotes == '\"' =>
          val quotesText = stringText(text)
          token = token :+("Quotes", quotesText)
          scanner(text.substring(quotesText.length), '_')

        case ident if ident.isLetter || ident.isDigit =>
          if (lastChar.isLetter || lastChar.isDigit) {
            token = token.dropRight(1) :+(token.last._1, token.last._2 + ident)
            scanner(text.tail, ident)
          } else {
            token = token :+("Ident", ident.toString)
            scanner(text.tail, ident)
          }

        case plus if plus.equals('+') =>
          token = token :+("Plus", plus.toString)
          scanner(text.tail, plus)

        case semicolon if semicolon.equals(';') =>
          token = token :+("semicolon", semicolon.toString)
          scanner(text.tail, semicolon)

        case coma if coma.equals(',') =>
          token = token :+("coma", coma.toString)
          scanner(text.tail, coma)

        case space if space.equals(' ') =>
          if (lastChar == ' ') {
            token = token.dropRight(1) :+(token.last._1, token.last._2 + space)
            scanner(text.tail, space)
          } else {
            token = token :+("Space", space.toString)
            scanner(text.tail, space)
          }

        case minus if minus.equals('-') =>
          token = token :+("Minus", minus.toString)
          scanner(text.tail, minus)

        case div if div.equals('/') =>
          if (text.length() > 1 && text(1) == '*') {
            val comText = multilineComment(text)
            token = token :+("Comment", comText)
            scanner(text.substring(comText.length), '_')
          }
          else if (text.length() > 1 && text(1) == '/') {
            val comText = singleLineComment(text)
            token = token :+("Comment", comText)
            try {
              scanner(text.substring(comText.length), '_')
            } catch {
              case ex: java.lang.StringIndexOutOfBoundsException => scanner("", '_')
            }
          } else {
            token = token :+("Div", div.toString)
            scanner(text.tail, div)
          }
        case multiply if multiply.equals('*') => {
          if (text.length() > 1 && text(1) == '*') {
            token = token :+("Power", "**")
            scanner(text.drop(2), multiply)
          }
          else {
            token = token :+("Multiply", multiply.toString)
            scanner(text.tail, multiply)
          }
        }

        case notDefined =>
          token = token :+("notDefined", notDefined.toString)
          scanner(text.tail, '_')

      }
    else
      token.map {
        case (_, value) if value.forall(Character.isDigit) => ("Number", value)
        case (_, value) if Dictionary.getSk.contains(value) => ("Sk", value)
        case (_, value) if Dictionary.getTypeOfVariable.contains(value) => ("Type", value)
        case (_, value) if Dictionary.getReservedWord.contains(value) => ("Reserved", value)
        case other => other
      }
  }
}