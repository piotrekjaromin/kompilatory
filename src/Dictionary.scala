object Dictionary {

  private val sk: Seq[String] = Seq("if", "for", "while", "do")

  private val bracket: Seq[Char] = Seq('{', '}', '[', ']', '(', ')')

  private val reservedWord: Seq[String] = Seq("abstract", "assert", "break", "case", "catch", "class", "const") ++
    Seq("continue", "default", "else", "enum", "extends", "final", "finally", "goto", "implements", "import") ++
    Seq("instanceof", "interface", "native", "new", "package", "private", "protected", "public", "return") ++
    Seq("static", "strictfp", "super", "switch", "synchronized", "this", "throw", "transient", "try", "void", "volatile")

  private val typeOfVariable: Seq[String] = Seq("byte", "short", "int", "long", "float", "double", "boolean", "char", "String")

  def getSk = sk

  def getReservedWord = reservedWord;

  def getTypeOfVariable = typeOfVariable;

  def getBracket = bracket
}