import scala.collection.BufferedIterator

/*
GameTree   	= '(' Node+ GameTree* ')'
Node       	= ';' Property*
Property   	= PropId PropVal+
PropId  		= Letter+
Letter   		= 'A'..'Z'
PropVal  		= '[' Text ']'
 */

type Tree[A]   = Node[A] // to separate the type from the constructor, cf. Haskell's Data.Tree
type Forest[A] = Seq[Tree[A]]
case class Node[A](root: A, forest: Forest[A] = Seq.empty[Tree[A]])

// A tree of nodes.
type SgfTree = Tree[SgfNode]

// A node is a property list, each key can only occur once.
// Keys may have multiple values associated with them.
type SgfNode = Map[String, Seq[String]]

class Sgf private (text: BufferedIterator[Char]):
  private def parseTree: Either[String, Option[SgfTree]] =
    if text.nextOption() != Some('(') then Left("tree missing")
    else
      for
        nodes <- parseNodes
        trees <- Seq
          .unfold(text.headOption)(c => Option.when(c == Some('('))((parseTree, text.headOption)))
          .partitionMap(identity) match
          case (h +: _, rights) => Left(h)
          case (_, rights)      => Right(nodes.foldRight(rights.flatten) { (root, forest) => List(Node(root, forest)) })

        _ = while text.headOption == Some(')') do text.next()
      yield trees.headOption

  private def parseNodes: Either[String, Seq[SgfNode]] =
    if text.nextOption() != Some(';') then Left("tree with no nodes")
    else
      for
        node  <- parseProperties
        nodes <- if text.headOption == Some(';') then parseNodes else Right(Seq.empty)
      yield node +: nodes

  private def parseProperties: Either[String, SgfNode] =
    for
      p     <- parseProperty
      props <- if text.headOption.exists(_.isLetter) then parseProperties else Right(Map.empty)
    yield (if p._1.isEmpty() then Map.empty else props + p)

  private def parseProperty: Either[String, (String, Seq[String])] =
    for
      id     <- parseId()
      values <- if id.isEmpty() then Right(Seq.empty) else parseValues
    yield (id, values)

  private def parseId(buf: StringBuilder = StringBuilder()): Either[String, String] =
    val c = text.head
    if !c.isLetter then Right(buf.mkString)
    else if c.isUpper then parseId(buf.append(text.next()))
    else Left("property must be in uppercase")

  private def parseValues: Either[String, Seq[String]] =
    if text.nextOption() != Some('[') then Left("properties without delimiter")
    else
      val v = parseValue()
      if text.headOption == Some('[') then parseValues.map(v +: _) else Right(Seq(v))

  private def parseValue(escaped: Boolean = false, buf: StringBuilder = StringBuilder()): String =
    val c = text.next()
    if !escaped && c == ']' then buf.mkString
    else
      if escaped then buf.deleteCharAt(buf.size - 1)
      // There is a test that requires escaped newline gets replaced with nothing.
      if escaped && c == '\n' then buf.append("")
      else if c.isWhitespace then buf.append(" ")
      else buf.append(c)
      parseValue(c == '\\' && !escaped, buf)

object Sgf:
  def parseSgf(text: String): Option[SgfTree] =
    val buf = Iterator.from(text).buffered
    new Sgf(buf).parseTree match
      case Right(tree) if !buf.hasNext => tree
      case Left(msg)                   => println(s"ERROR: $msg"); None
      case _                           => scala.sys.error(s"ERROR: remaining input ${buf.mkString}"); None
