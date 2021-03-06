package io.bigpanda.sherlock.command.model.command.json

import com.maraney.jackvm.Types._
import com.maraney.jackvm.{Parser, UnitSpec}

class ParserSpec extends UnitSpec {
  "The Parser" when {
    "parsing blank lines" must {
      "parse lines with whitespace" in {
        Parser.parse("        ") shouldEqual NoOp
      }
      "parse lines with no charactesr" in {
        Parser.parse("") shouldEqual NoOp
      }
      "ignore comments" in {
        Parser.parse("     //pomegranate") shouldEqual NoOp
      }
    }

    "ignore comments around instructions" in {
      Parser.parse("  add   //pomegranate") shouldEqual Add
    }

    "parsing arithmentic instructions" must {
      "parse simple arithmetic commands" in {
        Parser.parse("add") shouldEqual Add
        Parser.parse("sub") shouldEqual Sub
        Parser.parse("neg") shouldEqual Neg
        Parser.parse("eq") shouldEqual Eq
        Parser.parse("gt") shouldEqual Gt
        Parser.parse("lt") shouldEqual Lt
        Parser.parse("and") shouldEqual And
        Parser.parse("or") shouldEqual Or
        Parser.parse("not") shouldEqual Not
      }
    }

    "parsing stack instructions" must {
      "parse push instructions" in {
        Parser.parse("push constant 12345") shouldEqual Push(Constant, 12345.toShort)
        Parser.parse("push static 12345") shouldEqual Push(Static, 12345.toShort)
        Parser.parse("push this 12345") shouldEqual Push(This, 12345.toShort)
        Parser.parse("push local 12345") shouldEqual Push(Local, 12345.toShort)
        Parser.parse("push argument 12345") shouldEqual Push(Argument, 12345.toShort)
        Parser.parse("push that 12345") shouldEqual Push(That, 12345.toShort)
        Parser.parse("push pointer 12345") shouldEqual Push(Pointer, 12345.toShort)
        Parser.parse("push temp 12345") shouldEqual Push(Temp, 12345.toShort)
      }

      "parse pop instructions" in {
        Parser.parse("pop static 12345") shouldEqual Pop(Static, 12345.toShort)
        Parser.parse("pop this 12345") shouldEqual Pop(This, 12345.toShort)
        Parser.parse("pop local 12345") shouldEqual Pop(Local, 12345.toShort)
        Parser.parse("pop argument 12345") shouldEqual Pop(Argument, 12345.toShort)
        Parser.parse("pop that 12345") shouldEqual Pop(That, 12345.toShort)
        Parser.parse("pop pointer 12345") shouldEqual Pop(Pointer, 12345.toShort)
        Parser.parse("pop temp 12345") shouldEqual Pop(Temp, 12345.toShort)
      }
    }

    "parsing program flow instructions" must {
      "parse label instructions" in {
        Parser.parse("label o1:._.:1o") shouldEqual Label(LabelName("o1:._.:1o"))
        Parser.parse("label") shouldEqual Malformed
        Parser.parse("label ") shouldEqual Malformed
        Parser.parse("label 0") shouldEqual Malformed
        Parser.parse("label 0label") shouldEqual Malformed

      }
      "parse goto instructions" in {
        Parser.parse("goto o1:._.:1o") shouldEqual Goto(LabelName("o1:._.:1o"))
      }
      "parse if-goto instructions" in {
        Parser.parse("if-goto bob") shouldEqual IfGoto(LabelName("bob"))
        Parser.parse("if-goto o1:._.:1o") shouldEqual IfGoto(LabelName("o1:._.:1o"))
      }
    }

    "function calling instructions" must {
      "parse function instructions" in {
        Parser.parse("function o1:._.:1o 3") shouldEqual Function(FunctionName("o1:._.:1o"), 3)

        Parser.parse("function o1:._.:1o") shouldEqual Malformed
      }

      "parse call instructions" in {
        Parser.parse("call o1:._.:1o 3") shouldEqual Call(FunctionName("o1:._.:1o"), 3)
        Parser.parse("call o1:._.:1o") shouldEqual Malformed
      }

      "parse return instructions" in {
        Parser.parse("return") shouldEqual Return()
      }
    }
  }
}
