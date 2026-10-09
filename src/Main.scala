@main def hello(): Unit =
  val text =
    """
      |def a = \x y z => f x y (z a b)
      |
      |-- this is a comment
      |def b = let v = a b 42; v""".stripMargin
  val tokens = Lexer.lex(text)
  println(tokens.show(text))
