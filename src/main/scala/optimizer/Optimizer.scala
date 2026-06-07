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
          // Apply the optimization recursively and remove the binding if the variable is unused
          val (initializer, ts) = constantFoldRecursively(e.initializer, types)
          if (initializer.value.isInstanceOf[TermTree.IntegerLiteral]) {
            // If the initializer is a constant, substitute it in the body.
            val (substitutedBody, substitutedTypes) = constantPropagation(e.body, e.name, initializer, types)
            constantFoldRecursively(substitutedBody, types ++ ts ++ substitutedTypes)
          } else {
          val (body, us) = constantFoldRecursively(e.body, types)
          val updated = Syntax(TermTree.Binding(e.name, e.initializer, body), tree.span)
          deadCodeEliminate(updated) match
            case Some(s) => (s, Map(s -> types(tree)))
            case _ => (updated, (ts ++ us).updated(updated, types(tree)))
          }

        case e: TermTree.Conditional =>
          // apply the optimization recursively and eliminate the conditional
          // if the condition is a known boolean
          val (condition, ts) = constantFoldRecursively(e.condition, types)
          val (success, us) = constantFoldRecursively(e.success, types)
          val (failure, vs) = constantFoldRecursively(e.failure, types)
          val updated = Syntax(TermTree.Conditional(condition, success, failure), tree.span)
          deadCodeEliminate(updated) match
            case Some(s) => (s, Map(s -> types(tree)))
            case _ => (updated, (ts ++ us ++ vs).updated(updated, types(tree)))

        case e: TermTree.TermAbstraction =>
          val (body, ts) = constantFoldRecursively(e.body, types)
          val updated = Syntax(TermTree.TermAbstraction(e.parameter, e.ascription, body), tree.span)
          (updated, Map(updated -> types(tree)))

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

  private def substitute(tree: Syntax[TermTree], name: String, argument: Syntax[TermTree]): Syntax[TermTree] =
    tree.value match
      case TermTree.Variable(n) => if n == name then argument else tree // Beta reduction
      case TermTree.TermApplication(f, a) => Syntax(TermTree.TermApplication(substitute(f, name, argument), substitute(a, name, argument)), tree.span)
      case TermTree.TermAbstraction(p, t, b) => if p.value.name != name then Syntax(TermTree.TermAbstraction(p, t, substitute(b, name, argument)), tree.span) else tree
      case TermTree.Conditional(c, s, f) => Syntax(TermTree.Conditional(substitute(c, name, argument), substitute(s, name, argument), substitute(f, name, argument)), tree.span)
      case TermTree.TypeApplication(f, t) => Syntax(TermTree.TypeApplication(substitute(f, name, argument), t), tree.span)
      case TermTree.TypeAbstraction(t, b) => Syntax(TermTree.TypeAbstraction(t, substitute(b, name, argument)), tree.span)

        

    

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
  
  private def constantPropagation(body: Syntax[TermTree], name: Syntax[TermTree.Variable], value: Syntax[TermTree], types: TypedProgram.TypeAssignments): (Syntax[TermTree], TypedProgram.TypeAssignments) = 
    import TermTree.{Binding as F, Variable as V}
    body.value match
      case V(n) if n == name.value.name => (value, Map(value -> types(body)))
      case F(param, initializer, bodyExpr) if param.value.name != name.value.name =>
        val (substitutedInitializer, ts) = constantPropagation(initializer, name, value, types)
        val (substitutedBody, us) = constantPropagation(bodyExpr, name, value, types)
        val updated = Syntax(F(param, substitutedInitializer, substitutedBody), body.span)
        (updated, (ts ++ us).updated(updated, types(body)))
      case app: TermTree.TermApplication =>
        val (substitutedAbstraction, ts) = constantPropagation(app.abstraction, name, value, types)
        val (substitutedArgument, us) = constantPropagation(app.argument, name, value, types)
        val updated = Syntax(TermTree.TermApplication(substitutedAbstraction, substitutedArgument), body.span)
        (updated, (ts ++ us).updated(updated, types(body)))
      case _ => (body, Map(body -> types(body)))
  
  /** Replace variables with their constant values, if possible. */
  /* private def constantPropagation(tree: Syntax[TermTree], types: Map[Syntax[TermTree], Type]): Option[Syntax[TermTree]] =
    import TermTree.Binding as F
    tree.value match
      case F(name, initializer, body) =>
        // If the initializer is a constant, substitute it in the body.
        if (initializer.value.isInstanceOf[TermTree.IntegerLiteral]) {
          Some(substitute(body, name, initializer, types)._1)
        } else {
          constantFold(initializer).flatMap { foldedInitializer =>
            Some(substitute(body, name, foldedInitializer, types)._1)
          }
        }
      case _ => None */

end Optimizer

/** A pattern for recognizing integer constants. */
private object IntegerConstant:

  def unapply(s: Syntax[TermTree]): Option[Int] =
    s match
      case Syntax(TermTree.IntegerLiteral(n), _) => Some(n)
      case _ => None

end IntegerConstant
