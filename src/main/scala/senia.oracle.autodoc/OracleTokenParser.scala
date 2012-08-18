package senia.oracle.autodoc

import scala.util.parsing.combinator._

trait OracleTokenParser extends JavaTokenParsers {
  self =>

  override val skipWhitespace = false

  class Ins(s: String) {
    def i: Parser[String] = new Parser[String] {
      def apply(in: Input) = {
        val source = in.source
        val offset = in.offset
        val start = self.handleWhiteSpace(source, offset)
        var i = 0
        var j = start
        while (i < s.length && j < source.length && s.charAt(i).toLower == source.charAt(j).toLower) {
          i += 1
          j += 1
        }
        if (i == s.length)
          Success(source.subSequence(start, j).toString, in.drop(j - offset))
        else {
          val found = if (start == source.length()) "end of source" else "`" + source.charAt(start) + "'"
          Failure("`" + s + "' expected but " + found + " found", in.drop(start - offset))
        }
      }
    }
  }

  implicit def strToIns(s: String) = new Ins(s)

  def sp: Parser[String] = " " | "\t"

  def allSp: Parser[Space] = ( rep1(sp) ^^ {s => WhiteSpaces(s.mkString)} ) | commentBlock | lineComment | (("\r\n" | "\n") ^^ {t => EndLine})

  def allSps: Parser[List[Space]] = rep1(allSp)
  def allSpsOpt: Parser[List[Space]] = rep(allSp)

  def until(p: Parser[_]): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = self.handleWhiteSpace(source, offset)
      var j = start
      
      if (start >= source.length)
        Failure("until expected but end of source found", in.drop(start - offset))
      else {
        while (j < source.length && !p.apply(in.drop(j - offset)).successful) {
          j += 1
        }
      
        Success(source.subSequence(start, j).toString, in.drop(j - offset))
      }
    }
  }
  
  def commentBlock: Parser[CommentBlock] = "/*" ~> until("*/") <~ "*/" ^^ {s => CommentBlock(s)}

  def lineComment: Parser[LineComment] = "--" ~> ".*".r ~ ("\r\n" | "\n") ^^ {case a ~ _ => LineComment(a)}
}
