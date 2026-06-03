import com.dylibso.chicory
import com.dylibso.chicory.tools.wasm.Wat2Wasm

import java.io.File

import yafl.SourceFile
import yafl.emitter.Emitter
import yafl.optimizer.Optimizer
import yafl.parser.Parser
import yafl.typer.Typer

final class EmitterTests extends munit.FunSuite:

  test("argc"):
    val input = SourceFile("test", "#argc")
    val wasm = compile(input)
    val main = wasm.`export`("main")
    writeArguments(wasm, IArray(31, 11))
    assertEquals(main.apply()(0), 2L)

  test("argv"):
    val input = SourceFile("test", "(#argv 0) + (#argv 1)")
    val wasm = compile(input)
    val main = wasm.`export`("main")
    writeArguments(wasm, IArray(40, 2))
    assertEquals(main.apply()(0), 42L)

  test("integer addition"):
    val input = SourceFile("test", "40 + 2")
    val main = compile(input).`export`("main")
    assertEquals(main.apply()(0), 42L)

  test("integer substraction"):
    val input = SourceFile("test", "40 - 2")
    val main = compile(input).`export`("main")
    assertEquals(main.apply()(0), 38L)

  test("integer multiplication"):
    val input = SourceFile("test", "6 * 7")
    val main = compile(input).`export`("main")
    assertEquals(main.apply()(0), 42L)

  test("integer division"):
    val input = SourceFile("test", "40 / 2")
    val main = compile(input).`export`("main")
    assertEquals(main.apply()(0), 20L)

  test("integer equal comparison"):
    val input = SourceFile("test", "20 == 20")
    val main = compile(input).`export`("main")
    // 1L for true, 0L for false
    assertEquals(main.apply()(0), 1L)
    assertNotEquals(main.apply()(0), 0L)

  test("integer not equal comparison"):
    val input = SourceFile("test", "2005 != 2004")
    val main = compile(input).`export`("main")
    assertEquals(main.apply()(0), 1L)

  test("integer greater than comparison"):
    val input = SourceFile("test", "67 > 21")
    val main = compile(input).`export`("main")
    // 1L for true, 0L for false
    assertEquals(main.apply()(0), 1L)

  test("integer less than comparison"):
    val input = SourceFile("test", "20 < 67")
    val main = compile(input).`export`("main")
    // 1L for true, 0L for false
    assertEquals(main.apply()(0), 1L)

  /** Compiles `input` to a WebAssembly module and returns an instance of it. */
  private def compile(input: SourceFile): chicory.runtime.Instance =
    val program =  Optimizer.optimize(Typer.check(Parser.parse(input)))
    val binary = Wat2Wasm.parse(Emitter.emit(program))
    val m = chicory.wasm.Parser.parse(binary)
    chicory.runtime.Instance.builder(m).build()

  /** Initializes the command-line arguments of `wasm` to `values`. */
  private def writeArguments(wasm: chicory.runtime.Instance, values: IArray[Int]): Unit =
    val m = wasm.memory()
    m.writeI32(0, values.length)
    for i <- 0 until values.length do m.writeI32(4 + (i * 4), values(i))

end EmitterTests
