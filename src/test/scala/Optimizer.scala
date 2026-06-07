
import yafl.SourceFile
import yafl.optimizer.Optimizer
import yafl.parser.Parser
import yafl.syntax.{Syntax, TermTree}
import yafl.typer.{TypedProgram, Typer}

final class OptimizerTests extends munit.FunSuite:

  test("constant folding"):
    val optimized = optimize("1 + 2 + 3")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(6) => ()

  test("constant propagation"):
    val propagated = optimize("let x = 42; x + 1")
    (propagated.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(43) => ()

  test("normalization"):
    import TermTree.TermApplication as F
    import TermTree.TermAbstraction as A
    val optimized = optimize("(x : Int) => x + 1")
    (optimized.syntax.value : @unchecked) match
      case A(_, _, Syntax(F(lhs, Syntax(TermTree.Variable("x"), _)), _)) =>
        (lhs.value : @unchecked) match
          case F(_, Syntax(TermTree.IntegerLiteral(1), _)) => ()

  test("dead code — if true"):
    val optimized = optimize("if true then 1 else 2")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(1) => ()

  test("dead code — if false"):
    val optimized = optimize("if false then 1 else 2")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(2) => ()

  test("dead code — folded condition"):
    val optimized = optimize("if 5 == 5 then 1 else 2")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(1) => ()

  test("dead code — unused binding"):
    val optimized = optimize("let x = 99 ; 42")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(42) => ()

  test("inlining"):
    val optimized = optimize("((x: Int) => x + x) 5")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(10) => ()

  /** Compiles `input` to a WebAssembly module and returns an instance of it. */
  private def optimize(input: String): TypedProgram =
    Optimizer.optimize(Typer.check(Parser.parse(SourceFile("test", input))))

end OptimizerTests
