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

          // (Lines 34 to 42 were done with the help of a LLM)
          // Check if we can perform beta reduction
          val betaReduced: Option[Syntax[TermTree]] = f.value match
            case TermTree.TermAbstraction(param, _, body) =>
              Some(substitute(body, param.value.name, a))
            case _ => None

          betaReduced match
            case Some(reduced) =>
              // Re-optimize the beta-reduced term
              constantFoldRecursively(reduced, ts ++ us + (reduced -> types(tree)))
            case None =>
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
          val updated = Syntax(TermTree.Binding(e.name, initializer, body), tree.span)
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

  /** Substitutes all occurrences of a variable with a term. */
  private def substitute(tree: Syntax[TermTree], name: String, argument: Syntax[TermTree]): Syntax[TermTree] =
    if !occursIn(name, tree) then return tree

    tree.value match
      case TermTree.Variable(n) => if n == name then argument else tree // Beta reduction
      case TermTree.TermApplication(f, a) => Syntax(TermTree.TermApplication(substitute(f, name, argument), substitute(a, name, argument)), tree.span)
      case TermTree.TermAbstraction(p, t, b) => if p.value.name != name 
        then Syntax(TermTree.TermAbstraction(p, t, substitute(b, name, argument)), tree.span) 
        else tree
      case TermTree.TypeAbstraction(t, b) => Syntax(TermTree.TypeAbstraction(t, substitute(b, name, argument)), tree.span)
      case TermTree.Conditional(c, s, f) => Syntax(TermTree.Conditional(substitute(c, name, argument), substitute(s, name, argument), substitute(f, name, argument)), tree.span)
      case TermTree.TypeApplication(f, t) => Syntax(TermTree.TypeApplication(substitute(f, name, argument), t), tree.span)
      case TermTree.Binding(n, init, body) => if n.value.name != name 
        then Syntax(TermTree.Binding(n, substitute(init, name, argument), substitute(body, name, argument)), tree.span) 
        else Syntax(TermTree.Binding(n, substitute(init, name, argument), body), tree.span)
      case TermTree.RecursiveAbstraction(n, t, definition) => if n.value.name != name
        then Syntax(TermTree.RecursiveAbstraction(n, t, substitute(definition, name, argument)), tree.span)
        else Syntax(TermTree.RecursiveAbstraction(n, t, definition), tree.span)
      case _ => tree

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
  
  /** Replace variables with their constant values, if possible. Guided by claude.*/
  private def constantPropagation(body: Syntax[TermTree], name: Syntax[TermTree.Variable], value: Syntax[TermTree], types: TypedProgram.TypeAssignments): (Syntax[TermTree], TypedProgram.TypeAssignments) = 
    body.value match
      case TermTree.Variable(n) if n == name.value.name => (value, Map(value -> types(body)))

      case TermTree.Binding(param, initializer, bodyExpr) if param.value.name != name.value.name =>
        val (substitutedInitializer, ts) = constantPropagation(initializer, name, value, types)
        val (substitutedBody, us) = constantPropagation(bodyExpr, name, value, types)
        val updated = Syntax(TermTree.Binding(param, substitutedInitializer, substitutedBody), body.span)
        (updated, (ts ++ us).updated(updated, types(body)))

      case TermTree.Binding(param, initializer, bodyExpr) =>
        val (substitutedInitializer, ts) = constantPropagation(initializer, name, value, types)
        val updated = Syntax(TermTree.Binding(param, substitutedInitializer, bodyExpr), body.span)
        (updated, ts.updated(updated, types(body)))

      case TermTree.TermApplication(abstraction, argument) =>
        val (substitutedAbstraction, ts) = constantPropagation(abstraction, name, value, types)
        val (substitutedArgument, us) = constantPropagation(argument, name, value, types)
        val updated = Syntax(TermTree.TermApplication(substitutedAbstraction, substitutedArgument), body.span)
        (updated, (ts ++ us).updated(updated, types(body)))

      case TermTree.TermAbstraction(param, ascription, bodyExpr) if param.value.name != name.value.name =>
        val (substitutedBody, ts) = constantPropagation(bodyExpr, name, value, types)
        val updated = Syntax(TermTree.TermAbstraction(param, ascription, substitutedBody), body.span)
        (updated, ts.updated(updated, types(body)))

      case TermTree.TypeAbstraction(param, bodyExpr) =>
        val (substitutedBody, ts) = constantPropagation(bodyExpr, name, value, types)
        val updated = Syntax(TermTree.TypeAbstraction(param, substitutedBody), body.span)
        (updated, ts.updated(updated, types(body)))

      case TermTree.TypeApplication(abstraction, argument) =>
        val (substitutedAbstraction, ts) = constantPropagation(abstraction, name, value, types)
        val updated = Syntax(TermTree.TypeApplication(substitutedAbstraction, argument), body.span)
        (updated, ts.updated(updated, types(body)))
      
      case TermTree.Conditional(condition, success, failure) =>
        val (substitutedCondition, ts) = constantPropagation(condition, name, value, types)
        val (substitutedSuccess, us) = constantPropagation(success, name, value, types)
        val (substitutedFailure, vs) = constantPropagation(failure, name, value, types)
        val updated = Syntax(TermTree.Conditional(substitutedCondition, substitutedSuccess, substitutedFailure), body.span)
        (updated, (ts ++ us ++ vs).updated(updated, types(body)))

      case TermTree.RecursiveAbstraction(param, ascription, definition) if param.value.name != name.value.name =>
        val (substitutedDefinition, ts) = constantPropagation(definition, name, value, types)
        val updated = Syntax(TermTree.RecursiveAbstraction(param, ascription, substitutedDefinition), body.span)
        (updated, ts.updated(updated, types(body)))
      
      case _ => (body, Map(body -> types(body)))

end Optimizer

/** A pattern for recognizing integer constants. */
private object IntegerConstant:

  def unapply(s: Syntax[TermTree]): Option[Int] =
    s match
      case Syntax(TermTree.IntegerLiteral(n), _) => Some(n)
      case _ => None

end IntegerConstant
