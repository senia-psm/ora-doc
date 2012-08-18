package senia.oracle.autodoc

sealed abstract class Space(val str: String)
sealed abstract class Comment(str: String) extends Space(str) {
  import org.clapper.markwrap.{MarkWrap, MarkupType}
//  import OracleCommentParser.Success
  private[this] val parser = MarkWrap.parserFor(MarkupType.Markdown)

  def strip(s: String) = s.replace("\r\n", "\n").replaceAll("""^\**\n*""", "").replaceAll("""\n*\**$""", "").split("\n").map{ _.replaceAll("""^\t+""", "")}.mkString("\n")
  
  val notEmpty: PartialFunction[CommentElement, CommentElement] = {
    case a: Author => a
    case a: DbVersion => a
    case a: Prerequisites => a
    case a @ TextBlock(s) if s.replaceAll("""\s+""", "").length > 0 => a
  }
  
  def toXml() ={ val p = new OracleCommentParser(); p(str) match {
    case p.Success((None, cs), _) if cs.headOption.collect{ case TextBlock(t) => t }.map{ _.startsWith("*") }.getOrElse(false) => 
      <comment mode="markdown">
        {
          cs.map{
            case TextBlock(s) => TextBlock(parser.parseToHTML(io.Source.fromString(strip(s))))
            case x => x
          }.
          collect{ notEmpty }.
          map{ _.toXml }
        }
      </comment>
    case p.Success((_, cs), _) => 
      <comment>
        {
          cs.map{
            case TextBlock(s) => TextBlock(strip(s))
            case x => x
          }.
          collect{ notEmpty }.
          map{ _.toXml }
        }
      </comment>
    case _ =>
      <comment mode="parse_error">
        { TextBlock(strip(str)).toXml }
      </comment>
  }}
}
case class WhiteSpaces(s: String) extends Space(s)
case class CommentBlock(s: String) extends Comment(s)
case class LineComment(s: String) extends Comment(s)
case object EndLine extends Space("\n")
