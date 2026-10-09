object Common:
  type Name = String

  final case class Span(start: Int, end: Int):
    inline def extract(text: String): String = text.substring(start, end)
