// https://github.com/scala/scala-parser-combinators
import scala.util.parsing.combinator.RegexParsers

/*
GameTree   	= '(' Node+ GameTree* ')'
Node       	= ';' Property*
Property   	= PropId PropVal+
PropId  		= Letter+
Letter   		= 'A'..'Z'
PropVal  		= '[' Text ']'
 */
object Sgf extends RegexParsers:

  type Tree[A]   = Node[A] // to separate the type from the constructor, cf. Haskell's Data.Tree
  type Forest[A] = Seq[Tree[A]]
  case class Node[A](root: A, forest: Forest[A] = Seq.empty[Tree[A]])

  // A tree of nodes.
  type SgfTree = Tree[SgfNode]

  // A node is a property list, each key can only occur once.
  // Keys may have multiple values associated with them.
  type SgfNode = Map[String, Seq[String]]

  def parseSgf(text: String): Option[SgfTree] =
    parse(parseTree, text) match
      case Success(tree, _) => tree
      case Failure(msg, _)  => None
      case Error(msg, _)    => scala.sys.error(s"ERROR: $msg"); None

  def parseTree: Parser[Option[SgfTree]] =
    '(' ~> rep(parseNode) ~ rep(parseTree) <~ ')' ^^ { case nodes ~ tree =>
      nodes.foldRight(tree.flatten) { (root, forest) => List(Node(root, forest)) }.headOption
    }

  def parseNode: Parser[SgfNode] = ';' ~> opt(parseProperties) ^^ { _.getOrElse(Map.empty) }

  def parseProperties: Parser[SgfNode] = rep(parseProperty) ^^ { _.toMap }

  def parseProperty: Parser[(String, Seq[String])] = parseId ~ parseValues ^^ { case k ~ v => (k, v) }

  def parseId: Parser[String] = log("[A-Z]+".r)("id")

  def parseValues: Parser[Seq[String]] = rep1('[' ~> log(parseValue())("value") <~ ']')

  def parseValue(escaped: Boolean = false, buf: StringBuilder = StringBuilder()): Parser[String] =
    acceptIf(escaped || _ != ']')(c => s"$c") >> { c =>
      if escaped then buf.deleteCharAt(buf.size - 1)
      // There is a test that requires escaped newline gets replaced with nothing.
      if escaped && c == '\n' then buf.append("")
      else if c.isWhitespace then buf.append(" ")
      else buf.append(c)
      parseValue(c == '\\' && !escaped, buf)
    } | success(buf.mkString)
