package senia.oracle.autodoc

import scala.util.parsing.combinator._

class OracleParser extends OracleTokenParser {

  def file: Parser[PackageHead] = packageHead <~ packageBody

  def packageHead: Parser[PackageHead] =
    packageHeadTitle ~
    rep(packageHeadElement) <~
    allSpsOpt ~
    "end".i ~
    opt(allSps ~ ident) ~
    ";" ~ allSps ~ "/" ~ allSps ^^
    { case t ~ ps => PackageHead.moveComment(t, ps) }

  def packageHeadTitle: Parser[PackageHeadTitle] =
    allSpsOpt ~
    "create".i ~
    allSps ~
    "or".i ~
    allSps ~
    "replace".i ~
    allSps ~
    "package".i ~
    allSps ~
    ident ~
    allSps ~
    ("is".i | "as".i) ~
    allSps ^^
    { case c1 ~ _ ~ c2 ~ _ ~ c3 ~ _ ~ c4 ~ _ ~ c5 ~ name ~ c6 ~ _ ~ c7 =>
      PackageHeadTitle(name, (c1 ++: c2 ++: c3 ++: c4 ++: c5 ++: c6).collect{ case c: Comment => c }, c7)}

  def packageHeadElement: Parser[PackageHeadElement] = procedureHead | functionHead | customType

  def customType: Parser[CustomType] =
    allSpsOpt ~
    "type".i ~
    allSps ~
    ident <~
    allSps ~
    """([^;]|\n)*""".r ~
    ";" ^^ {case c ~ _ ~ _ ~ name => CustomType(name, c.collect{ case c: Comment => c})}

  def functionHead: Parser[FunctionHead] =
    allSpsOpt ~
    "function".i ~
    allSps ~
    ident ~
    allSpsOpt ~
    "(" ~
    opt(rep(startParam) ~ parameter ^^ {case a ~ b => a :+ b}) ~
    allSpsOpt ~
    ")" ~
    allSpsOpt ~
    "return".i ~
    allSps ~
    ident <~
    rep(allSps ~ ("pipelined".i | "deterministic".i)) ~
    allSpsOpt ~
    ";" ^^
    { case c1 ~ _ ~ c2 ~ name ~ c3 ~ _ ~ params ~ _ ~ _ ~ _ ~ _ ~ _ ~ resultType =>
      FunctionHead(name, (c1 ++: c2 ++: c3).collect{ case c: Comment => c}, params.getOrElse(Nil), resultType)}

  def procedureHead: Parser[ProcedureHead] =
    allSpsOpt ~
    "procedure".i ~
    allSps ~
    ident ~
    allSpsOpt ~
    "(" ~
    opt(rep(startParam) ~ parameter ^^ {case a ~ b => a :+ b}) <~
    allSpsOpt ~
    ")" ~
    allSpsOpt ~
    ";" ^^
    { case c1 ~ _ ~ c2 ~ name ~ c3 ~ _ ~ params =>
      ProcedureHead(name, (c1 ++: c2 ++: c3).collect{ case c: Comment => c}, params.getOrElse(Nil))}

  def parameter: Parser[Parameter] = 
    allSpsOpt ~
    """[a-zA-Z_](\w|[а-я])*""".r ~//ident ~ !!!!!!!!!!!!!!!!!!!!!1
    allSps ~
    opt((("in".i ~ allSps ~ "out".i ^^ { _ => "in out"}) | ("in".i ^^ {_ => "in"} ) | ("out".i ^^ {_ => "out"} ) ) ~ allSps) ~
    (repsep(ident, ".") ^^ {t => t.mkString(".")}) ~
    opt(allSpsOpt ~ (":=" | "default".i) ~ allSpsOpt ~ (opt("'") ~ """\w*""".r ~ opt("'") ^^ { case a ~ b ~ c => a.mkString + b + c.mkString})) ~
    allSpsOpt ^^
    { case c1 ~ name ~ c2 ~ direction ~ oraType ~ default ~ c3 =>
      Parameter(name, oraType, direction.map{_._1}, default.map{case _ ~ _ ~ _ ~ d => d}, (c1 ++: c2 ++: direction.map{_._2}.getOrElse(Nil) ++: default.map{case a ~ _ ~ b ~ _ => a ++: b} ++: c3).collect{ case c: Comment => c})}

  def startParam: Parser[Parameter] = 
    parameter ~
    allSpsOpt ~
    "," ~
    allSpsOpt ^^
    { case p ~ c1 ~ _ ~ c2 =>
      Parameter(p.name, p.oraType, p.direction, p.default, p.comments ++: (c1 ++: c2).collect{ case c: Comment => c})}

  def packageBody: Parser[String] =
    "create".i ~
    allSps ~
    "or".i ~
    allSps ~
    "replace".i ~
    allSps ~
    "package".i ~
    allSps ~
    "body".i ~
    allSps ~
    rep("""(.|\r|\n)""".r) ^^ { case a ~ b => a + b.mkString }
  
  def apply(s: String) = parseAll(file, s)
}

