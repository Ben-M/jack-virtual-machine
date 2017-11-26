package com.maraney.jackvm

import com.maraney.jackvm.Types._

object Coder {

  case class CoderState(comparisonCount: Integer, assembler: String)

  def code(instruction: Instruction, state: CoderState, fileName: String) = {
    instruction match {
      case Add =>
        val asm = annotate("add") + applyTwoParamArithmetic("D+M")
        append(asm, state)
      case Sub =>
        val asm = annotate("sub") + applyTwoParamArithmetic("M-D")
        append(asm, state)
      case Neg =>
        val asm = annotate("neg") + setMToNextParam + setVal("-M")
        append(asm, state)
      case Eq =>
        val asm =
          annotate("eq") + applyComparison("EQ", state.comparisonCount)
        appendAndInc(asm, state)
      case Gt =>
        val asm =
          annotate("gt") + applyComparison("GT", state.comparisonCount)
        appendAndInc(asm, state)
      case Lt =>
        val asm =
          annotate("lt") + applyComparison("LT", state.comparisonCount)
        appendAndInc(asm, state)
      case And =>
        val asm = annotate("and") + applyTwoParamArithmetic("D&M")
        append(asm, state)
      case Or =>
        val asm = annotate("or") + applyTwoParamArithmetic("D|M")
        append(asm, state)
      case Not =>
        val asm = annotate("not") + setMToNextParam + setVal("!M")
        append(asm, state)
      case Push(Constant, i) =>
        val asm = annotate("push constant " + i) + setDTo(i) + setVal("D")
        append(asm, state)
      case Push(Local, i) =>
        val asm = annotate("push local " + i) + setAToRef("LCL", i)+ setDToM + pushD
        append(asm, state)
      case Pop(Local, i) =>
        val asm = annotate("pop local " + i) + setDToTopValueOnStack + setAToRef("LCL", i) + popD
        append(asm, state)
      case Push(Argument, i) =>
        val asm = annotate("push argument " + i) + setAToRef("ARG", i)+ setDToM + pushD
        append(asm, state)
      case Pop(Argument, i) =>
        val asm = annotate("pop argument " + i) + setDToTopValueOnStack + setAToRef("ARG", i) + popD
        append(asm, state)
      case Push(This, i) =>
        val asm = annotate("push this " + i) + setAToRef("THIS", i)+ setDToM + pushD
        append(asm, state)
      case Pop(This, i) =>
        val asm = annotate("pop this " + i) + setDToTopValueOnStack + setAToRef("THIS", i) + popD
        append(asm, state)
      case Push(That, i) =>
        val asm = annotate("push that " + i) + setAToRef("THAT", i)+ setDToM + pushD
        append(asm, state)
      case Pop(That, i) =>
        val asm = annotate("pop that " + i) + setDToTopValueOnStack + setAToRef("THAT", i) + popD
        append(asm, state)
      case Push(Pointer, i) =>
        val asm = annotate("push pointer " + i) + setAToDirect(3, i)+ setDToM + pushD
        append(asm, state)
      case Pop(Pointer, i) =>
        val asm = annotate("pop pointer " + i) + setDToTopValueOnStack + setAToDirect(3, i) + popD
        append(asm, state)
      case Push(Temp, i) =>
        val asm = annotate("push temp " + i) + setAToDirect(5, i)+ setDToM + pushD
        append(asm, state)
      case Pop(Temp, i) =>
        val asm = annotate("pop temp " + i) + setDToTopValueOnStack + setAToDirect(5, i) + popD
        append(asm, state)
      case Push(Static, i) =>
        val asm = annotate("push static " + i) + setAToStatic(fileName, i) + setDToM + pushD
        append(asm, state)
      case Pop(Static, i) =>
        val asm = annotate("pop static " + i) + setDToTopValueOnStack + setAToStatic(fileName, i) + popD
        append(asm, state)

    }
  }

  private val setDToTopValueOnStack = {
    """@SP
      |A=M
      |A=A-1
      |D=M
      |""".stripMargin
  }

  private def setAToDirect(start: Short, i: Short) =
    "@" + (start + i) + "\n"

  private def setAToStatic(fileName: String, i: Short) =
    "@" + fileName + "." + i+ "\n"

  private def setAToRef(symbol: String, i: Short) =
    "@" + symbol +
      "\nA=M\n" +
      incA(i)

  private def incA(times: Int) =
    "A=A+1\n" * times


  private val pushD =
    """@SP
      |A=M
      |""".stripMargin + setVal("D")

  private val popD =
    """M=D
      |@SP
      |M=M-1""".stripMargin


  private def annotate(name: String) =
    "// " + name + "\n"

  private val setMToNextParam =
    """@SP
      |M=M-1
      |A=M
      |""".stripMargin

  private val setDToM = "D=M\n"
  private def setDTo(value: Int) =
    "@" + value +
      """
      |D=A
      |@SP
      |A=M
      |""".stripMargin

  private def setVal(value: String) =
    "M=" + value + """
                     |@SP
                     |M=M+1""".stripMargin

  private def setMToComparison(cmp: String, comparisonCount: Int) =
    ("""D=M-D
      |@CMP.""" + comparisonCount + """.TRUE
      |D;J""" + cmp + """
      |@CMP.""" + comparisonCount + """.END
      |D=0;JMP
      |(CMP.""" + comparisonCount + """.TRUE)
      |D=-1
      |(CMP.""" + comparisonCount + """.END)
      |@SP
      |A=M
      |""").stripMargin + setVal("D")

  private def applyComparison(comparison: String, comparisonCount: Int) =
    setMToNextParam +
      setDToM +
      setMToNextParam +
      setMToComparison(comparison, comparisonCount)

  private def applyTwoParamArithmetic(arithmetic: String) =
    setMToNextParam +
      setDToM +
      setMToNextParam +
      setVal(arithmetic)

  private def append(asm: String, state: CoderState) = {
    state.copy(assembler = state.assembler + asm + "\n")
  }

  private def appendAndInc(asm: String, state: CoderState) = {
    append(asm, state).copy(comparisonCount = state.comparisonCount + 1)
  }
}
