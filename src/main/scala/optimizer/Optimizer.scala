package yafl.optimizer

import yafl.syntax.{InfixOperator, Syntax, TermTree}
import yafl.typer.{Type, TypedProgram}

object Optimizer:

  /** Returns `program` optimized. */
  def optimize(program: TypedProgram): TypedProgram =
    val (optimized, updated) = constantFoldRecursively(program.syntax, program.types)
    TypedProgram(optimized, updated)

  /** Substitutes constant expressions in `tree` with their results, returning an updated syntax
    * tree along with a map from each term to its type. Also applies normalization and dead code
    * elimination.
    */
  private def constantFoldRecursively(
      tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): (Syntax[TermTree], TypedProgram.TypeAssignments) = {
    constantFold(tree) match
      case Some(s) =>
        // Constant folding succeeded; return the updated tree.
        (s, Map(s -> types(tree)))

      case _ => tree.value match
        case e: TermTree.TermApplication =>
          // Apply the optimization recursively.
          val (f, ts) = constantFoldRecursively(e.abstraction, types)
          val (a, us) = constantFoldRecursively(e.argument, types)
          val updated = Syntax(TermTree.TermApplication(f, a), tree.span)

          // Fold or normalize the result if possible.
          constantFold(updated).orElse(normalize(updated)) match
            case Some(s) => (s, Map(s -> types(tree)))
            case _ => (updated, (ts ++ us).updated(updated, types(tree)))

        case e: TermTree.Binding =>
          val (initializer, ts) = constantFoldRecursively(e.initializer, types)
          val (body, us) = constantFoldRecursively(e.body, types)
          val updated = Syntax(TermTree.Binding(e.name, initializer, body), tree.span)
          deadCodeEliminate(updated) match
            case Some(s) => (s, Map(s -> types(tree)))
            case _ => (updated, (ts ++ us).updated(updated, types(tree)))

        case e: TermTree.Conditional =>
          val (condition, ts) = constantFoldRecursively(e.condition, types)
          val (success, us) = constantFoldRecursively(e.success, types)
          val (failure, vs) = constantFoldRecursively(e.failure, types)
          val updated = Syntax(TermTree.Conditional(condition, success, failure), tree.span)
          deadCodeEliminate(updated) match
            case Some(s) => (s, Map(s -> types(tree)))
            case _ => (updated, (ts ++ us ++ vs).updated(updated, types(tree)))

        case _ =>
          (tree, Map(tree -> types(tree)))
  }

  /** Eliminates dead code: removes conditionals with known condition and variable that are never used.*/
  private def deadCodeEliminate(tree: Syntax[TermTree]): Option[Syntax[TermTree]] =
    tree.value match
      case TermTree.Conditional(Syntax(TermTree.BooleanLiteral(true), _), success, _) =>
        Some(success)
      case TermTree.Conditional(Syntax(TermTree.BooleanLiteral(false), _), _, failure) =>
        Some(failure)
      case TermTree.Binding(name, _, body) if !occursIn(name.value.name, body) =>
        Some(body)
      case _ => None

  /** Returns true if the variable is used anywhere in the tree. */
  private def occursIn(name: String, tree: Syntax[TermTree]): Boolean =
    tree.value match
      case TermTree.Variable(n) => n == name
      case TermTree.TermApplication(f, a) => occursIn(name, f) || occursIn(name, a)
      case TermTree.TermAbstraction(p, _, b) => p.value.name != name && occursIn(name, b)
      case TermTree.TypeAbstraction(_, b) => occursIn(name, b)
      case TermTree.TypeApplication(f, _) => occursIn(name, f)
      case TermTree.Conditional(c, s, f) => occursIn(name, c) || occursIn(name, s) || occursIn(name, f)
      case TermTree.Binding(n, init, body) => occursIn(name, init) || (n.value.name != name && occursIn(name, body))
      case TermTree.RecursiveAbstraction(n, _, definition) => n.value.name != name && occursIn(name, definition)
      case _ => false

  /** Returns a normalized form by moving constants to the left*/
  private def normalize(tree: Syntax[TermTree]): Option[Syntax[TermTree]] =
    import TermTree.TermApplication as F
    tree.value match
      case F(Syntax(F(operator, lhs), inner), rhs) if !lhs.value.isInstanceOf[TermTree.IntegerLiteral] && rhs.value.isInstanceOf[TermTree.IntegerLiteral] =>
        Some(Syntax(F(Syntax(F(operator, rhs), inner), lhs), tree.span))
      case _ => None

  /** Returns a literal denoting the result of `tree` iff it represents a constant expression. */
  private def constantFold(tree: Syntax[TermTree]): Option[Syntax[TermTree]] =
    import TermTree.TermApplication as F
    tree.value match
      case F(Syntax(F(InfixOperator(f), IntegerConstant(lhs)), _), IntegerConstant(rhs)) =>
        val n = f match
          // Arithmetic (returns an Integer)
          case InfixOperator.Add => lhs + rhs
          case InfixOperator.Sub => lhs - rhs
          case InfixOperator.Mul => lhs * rhs
          case InfixOperator.Div => lhs / rhs
          // Comparison (returns a bool)
          case InfixOperator.Great => lhs > rhs
          case InfixOperator.Less =>  lhs < rhs
          case InfixOperator.Equal => lhs == rhs
          case InfixOperator.NotEqual => lhs != rhs

        n match 
          case i: Int => Some(Syntax(TermTree.IntegerLiteral(i), tree.span))
          case b: Boolean => Some(Syntax(TermTree.BooleanLiteral(b), tree.span))

      case _ => None

end Optimizer

/** A pattern for recognizing integer constants. */
private object IntegerConstant:

  def unapply(s: Syntax[TermTree]): Option[Int] =
    s match
      case Syntax(TermTree.IntegerLiteral(n), _) => Some(n)
      case _ => None

end IntegerConstant
