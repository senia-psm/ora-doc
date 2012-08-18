package senia.oracle.autodoc

case class PackageHeadTitle(name: String, comments: List[Comment], lastComments: List[Space])

sealed abstract class PackageHeadElement(val elName: String, val elComments: List[Comment]) {
  def toXml(): xml.Elem
}

case class ProcedureHead(name: String, comments: List[Comment], params: List[Parameter]) extends PackageHeadElement(name, comments) {
  def toXml() =
    <item class="procedure" name={ name }>
      <comments>{ comments.map{_.toXml} }</comments>
      <params>{ params.map{_.toXml} }</params>
    </item>
}
case class FunctionHead(name: String, comments: List[Comment], params: List[Parameter], resultType: String) extends PackageHeadElement(name, comments) {
  def toXml() =
    <item class="function" name={ name }>
      <comments>{ comments.map{_.toXml} }</comments>
      <params>{ params.map{_.toXml} }</params>
      <result_type>{ resultType }</result_type>
    </item>
}
case class CustomType(name: String, comments: List[Comment]) extends PackageHeadElement(name, comments) {
  def toXml() =
    <item class="type" name={ name }>
      <comments>{ comments.map{_.toXml} }</comments>
    </item>
}
case class Parameter(name: String, oraType: String, direction: Option[String], default: Option[String], comments: List[Comment]) {
  def toXml() =
    <parameter name={ name }>
      <ora_type>{ oraType }</ora_type>
      {if (direction isDefined) <direction>{ direction get }</direction> }
      {if (default isDefined) <default>{ default get }</default> }
      <comments>{ comments.map{_.toXml} }</comments>
    </parameter>
}
case class PackageHead(title: PackageHeadTitle, procedures: List[PackageHeadElement]){
   def toXml() = <package name={ title.name }><comments>{ title.comments.map{_.toXml} }</comments><items>{ procedures.map{_.toXml} }</items></package>
}
object PackageHead{
  def moveComment(title: PackageHeadTitle, procedures: List[PackageHeadElement]): PackageHead = 
    if (procedures.isEmpty || procedures.head.elComments.nonEmpty)
      new PackageHead(
        PackageHeadTitle(title.name, title.comments ++: title.lastComments.collect{case c: Comment => c}, Nil),
        procedures
      )
    else {
      val (firstElementComment, rest) =
      title.
        lastComments.
        collect {
          case c: Comment => c
          case EndLine => EndLine
        }.
        reverse match {
          case (c: Comment) :: tail => (Some(c), tail.reverse)
          case EndLine :: (c: Comment) :: tail => (Some(c), tail.reverse)
          case x => (None, x.reverse)
        }
      
      new PackageHead(
        PackageHeadTitle(title.name, title.comments ++: rest.collect{case c: Comment => c}, Nil),
        procedures match {
          case (head: ProcedureHead) :: tail => head.copy(comments = firstElementComment ++: Nil) :: tail
          case (head: FunctionHead) :: tail => head.copy(comments = firstElementComment ++: Nil) :: tail
          case (head: CustomType) :: tail => head.copy(comments = firstElementComment ++: Nil) :: tail
          case Nil => sys.error("should never get here")
        }
      )
    }
}

