package yafl.syntax

/** An operator applied with infix notation (e.g., `+` in `1 + 2`). */
enum InfixOperator:

  case Add, Sub, Mul, Div, Great, Less, Equal, NotEqual

object InfixOperator:

  def unapply(s: Syntax[TermTree]): Option[InfixOperator] =
    s match
      case Syntax(TermTree.Variable(n), _) => n match
        case "infix+" => Some(Add)
        case "infix-" => Some(Sub)
        case "infix*" => Some(Mul)
        case "infix/" => Some(Div)
        case "infix>" => Some(Great)
        case "infix<" => Some(Less)
        case "infix==" => Some(Equal)
        case "infix!=" => Some(NotEqual)
        case _ => None
      case _ => None

end InfixOperator
