package senia.oracle.autodoc

case object Header
sealed abstract class CommentElement { def toXml(): xml.Elem }
case class Author(text: String) extends CommentElement { def toXml() = <author>{ text }</author> }
case class DbVersion(text: String) extends CommentElement { def toXml() = <db_version>{ text }</db_version> }
case class Prerequisites(text: String) extends CommentElement { def toXml() = <prerequisites>{ text }</prerequisites> }
case class TextBlock(text: String) extends CommentElement { def toXml() = <text_block>{ text }</text_block> }
