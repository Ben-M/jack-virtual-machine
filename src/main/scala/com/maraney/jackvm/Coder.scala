package com.maraney.jackvm

import com.maraney.jackvm.Types._

object Coder {

  case class CoderState(comparisonCount: Integer, assembler: String)

  def code(instruction: Instruction, state: CoderState) = {
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
    }
  }

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
