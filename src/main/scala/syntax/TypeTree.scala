package yafl.syntax

/** The payload of a syntax tree representing a type. */
sealed trait TypeTree

object TypeTree:

  /** A type variable. */
  case class Variable(name: String) extends TypeTree

  /** The type of a term abstraction (i.e., a function). */
  case class Arrow(
      domain: Syntax[TypeTree], codomain: Syntax[TypeTree]
  ) extends TypeTree

  /** The type of a type abstraction. */
  case class ForAll(
      parameter: Syntax[TypeTree.Variable], body: Syntax[TypeTree]
  ) extends TypeTree

end TypeTree
