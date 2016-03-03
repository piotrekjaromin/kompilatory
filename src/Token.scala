case class Token(token: Seq[(String, String)]) {

  override def toString() = token.map {
    case (key, value) => key + " -> " + value
  }.mkString("\n")

}

object Token {

  def apply(text: String): Token = Token(scanner(text, ' '))

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

  private def scanner(text: String, lastChar: Char): Seq[(String, String)] = {

    if (text.length() > 0)
      text.head match {

        case quotes if quotes == '\"' => insertAndCheckNext(text, "Quotes")

        case ident if ident.isLetter || ident.isDigit =>
          if (lastChar.isLetter || lastChar.isDigit) replaceAndCheckNext(text, ident)
          else insertAndCheckNext(text, "Ident", ident)

        case space if space.equals(' ') =>
          if (lastChar == ' ') replaceAndCheckNext(text, space)
          else insertAndCheckNext(text, "Space", space)

        case div if div.equals('/') =>
          if (text.length() > 1 && text(1) == '*') insertAndCheckNext(text, "MultilineComment")
          else if (text.length() > 1 && text(1) == '/') insertAndCheckNext(text, "SingleLineComment")
          else insertAndCheckNext(text, "Div", div)

        case multiply if multiply.equals('*') =>
          if (text.length() > 1 && text(1) == '*') insertAndTakeNext(text, "Power", "**", Some(multiply))
          else insertAndCheckNext(text, "Multiply", multiply)

        case plus if plus.equals('+') => insertAndCheckNext(text, "Plus", plus)

        case semicolon if semicolon.equals(';') => insertAndCheckNext(text, "Semicolon", semicolon)

        case coma if coma.equals(',') => insertAndCheckNext(text, "Coma", coma)

        case minus if minus.equals('-') => insertAndCheckNext(text, "Minus", minus)

        case dot if dot.equals('.') => insertAndCheckNext(text, "Dot", dot)

        case bracket if Dictionary.getBracket.contains(bracket)  => insertAndCheckNext(text, "Bracket", bracket)

        case notDefined => insertAndCheckNext(text, "notDefined", notDefined, Some('_'))
      }
    else token.map {

        case (_, value) if value.forall(Character.isDigit) => ("Number", value)
        case (_, value) if Dictionary.getSk.contains(value) => ("Sk", value)
        case (_, value) if Dictionary.getTypeOfVariable.contains(value) => ("Type", value)
        case (_, value) if Dictionary.getReservedWord.contains(value) => ("Reserved", value)
        case (key, value) if key.equals("Ident") && value(0).isDigit => ("Undefined", value)
        case other => other
      }
  }
}