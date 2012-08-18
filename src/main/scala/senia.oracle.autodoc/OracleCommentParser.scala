package senia.oracle.autodoc

import scala.util.parsing.combinator._

class OracleCommentParser extends OracleTokenParser {
  def header: Parser[Header.type] = rep1("*") ~ "HEADER".i ~ rep1("*") ^^ { _ => Header }
  
  def comment: Parser[(Option[Header.type], List[CommentElement])] = opt(header) ~! rep(textBlock | anyTag) ^^ { case h ~ ss => (h, ss)}

  def textBlock: Parser[TextBlock] = until(anyTag) ^? { case s if s.length > 0 => TextBlock(s)}
  def anyTag: Parser[CommentElement] = author | db_version | prerequisites
  
  def db_version: Parser[DbVersion] = tag("db_version") ^^ { s => DbVersion(s) }
  def author: Parser[Author] = tag("author") ^^ { s => Author(s) }
  def prerequisites: Parser[Prerequisites] = tag("prerequisites") ^^ { s => Prerequisites(s) }
  
  def tag(name: String): Parser[String] = "<" ~ name.i ~ ">" ~> until("</" ~ name.i ~ ">") <~ "</" ~ name.i ~ ">"
  
  def apply(s: String) = parseAll(comment, s)
}
