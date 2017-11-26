package io.bigpanda.sherlock.command.model.command.json

import com.maraney.jackvm.Coder.CoderState
import com.maraney.jackvm.Types._
import com.maraney.jackvm.{Coder, UnitSpec}

class CoderSpec extends UnitSpec {
  "The Coder" when {
    "coding arithmetic instructions" must {
      "code add" in {
        Coder.code(Add, CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// add
              |@SP
              |M=M-1
              |A=M
              |D=M
              |@SP
              |M=M-1
              |A=M
              |M=D+M
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }
      "code sub" in {
        Coder.code(Sub, CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// sub
              |@SP
              |M=M-1
              |A=M
              |D=M
              |@SP
              |M=M-1
              |A=M
              |M=M-D
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }
      "code neg" in {
        Coder.code(Neg, CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// neg
              |@SP
              |M=M-1
              |A=M
              |M=-M
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }
      "code eq and increment comparison count" in {
        Coder.code(Eq, CoderState(0, ""), "file") shouldEqual
          CoderState(
            1,
            """// eq
              |@SP
              |M=M-1
              |A=M
              |D=M
              |@SP
              |M=M-1
              |A=M
              |D=M-D
              |@CMP.0.TRUE
              |D;JEQ
              |@CMP.0.END
              |D=0;JMP
              |(CMP.0.TRUE)
              |D=-1
              |(CMP.0.END)
              |@SP
              |A=M
              |M=D
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }
      "code gt and increment comparison count" in {
        Coder.code(Gt, CoderState(0, ""), "file") shouldEqual
          CoderState(
            1,
            """// gt
              |@SP
              |M=M-1
              |A=M
              |D=M
              |@SP
              |M=M-1
              |A=M
              |D=M-D
              |@CMP.0.TRUE
              |D;JGT
              |@CMP.0.END
              |D=0;JMP
              |(CMP.0.TRUE)
              |D=-1
              |(CMP.0.END)
              |@SP
              |A=M
              |M=D
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }
      "code lt and increment comparison count" in {
        Coder.code(Lt, CoderState(0, ""), "file") shouldEqual
          CoderState(
            1,
            """// lt
              |@SP
              |M=M-1
              |A=M
              |D=M
              |@SP
              |M=M-1
              |A=M
              |D=M-D
              |@CMP.0.TRUE
              |D;JLT
              |@CMP.0.END
              |D=0;JMP
              |(CMP.0.TRUE)
              |D=-1
              |(CMP.0.END)
              |@SP
              |A=M
              |M=D
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }

      "code and" in {
        Coder.code(And, CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// and
              |@SP
              |M=M-1
              |A=M
              |D=M
              |@SP
              |M=M-1
              |A=M
              |M=D&M
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }
      "code or" in {
        Coder.code(Or, CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// or
              |@SP
              |M=M-1
              |A=M
              |D=M
              |@SP
              |M=M-1
              |A=M
              |M=D|M
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }
      "code not" in {
        Coder.code(Not, CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// not
              |@SP
              |M=M-1
              |A=M
              |M=!M
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }
    }
    "coding stack instructions" must {
      "code push constant" in {
        Coder.code(Push(Constant, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// push constant 3
              |@3
              |D=A
              |@SP
              |A=M
              |M=D
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }

      "code push local" in {
        Coder.code(Push(Local, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// push local 3
              |@LCL
              |A=M
              |A=A+1
              |A=A+1
              |A=A+1
              |D=M
              |@SP
              |A=M
              |M=D
              |@SP
              |M=M+1
              |""".stripMargin
          )

      }

      "code push argument" in {
        Coder.code(Push(Argument, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// push argument 3
              |@ARG
              |A=M
              |A=A+1
              |A=A+1
              |A=A+1
              |D=M
              |@SP
              |A=M
              |M=D
              |@SP
              |M=M+1
              |""".stripMargin
          )

      }

      "code push this" in {
        Coder.code(Push(This, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// push this 3
              |@THIS
              |A=M
              |A=A+1
              |A=A+1
              |A=A+1
              |D=M
              |@SP
              |A=M
              |M=D
              |@SP
              |M=M+1
              |""".stripMargin
          )

      }

      "code push that" in {
        Coder.code(Push(That, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// push that 3
              |@THAT
              |A=M
              |A=A+1
              |A=A+1
              |A=A+1
              |D=M
              |@SP
              |A=M
              |M=D
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }

      "code push pointer" in {
        Coder.code(Push(Pointer, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// push pointer 3
              |@6
              |D=M
              |@SP
              |A=M
              |M=D
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }

      "code push temp" in {
        Coder.code(Push(Temp, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// push temp 3
              |@8
              |D=M
              |@SP
              |A=M
              |M=D
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }

      "code push static" in {
        Coder.code(Push(Static, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// push static 3
              |@file.3
              |D=M
              |@SP
              |A=M
              |M=D
              |@SP
              |M=M+1
              |""".stripMargin
          )
      }

      "code pop local" in {
        Coder.code(Pop(Local, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// pop local 3
                |@SP
                |A=M
                |A=A-1
                |D=M
                |@LCL
                |A=M
                |A=A+1
                |A=A+1
                |A=A+1
                |M=D
                |@SP
                |M=M-1
                |""".stripMargin
          )
      }

      "code pop argument" in {
        Coder.code(Pop(Argument, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// pop argument 3
              |@SP
              |A=M
              |A=A-1
              |D=M
              |@ARG
              |A=M
              |A=A+1
              |A=A+1
              |A=A+1
              |M=D
              |@SP
              |M=M-1
              |""".stripMargin
          )
      }

      "code pop this" in {
        Coder.code(Pop(This, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// pop this 3
              |@SP
              |A=M
              |A=A-1
              |D=M
              |@THIS
              |A=M
              |A=A+1
              |A=A+1
              |A=A+1
              |M=D
              |@SP
              |M=M-1
              |""".stripMargin
          )
      }

      "code pop that" in {
        Coder.code(Pop(That, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// pop that 3
              |@SP
              |A=M
              |A=A-1
              |D=M
              |@THAT
              |A=M
              |A=A+1
              |A=A+1
              |A=A+1
              |M=D
              |@SP
              |M=M-1
              |""".stripMargin
          )
      }

      "code pop pointer" in {
        Coder.code(Pop(Pointer, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// pop pointer 3
              |@SP
              |A=M
              |A=A-1
              |D=M
              |@6
              |M=D
              |@SP
              |M=M-1
              |""".stripMargin
          )
      }

      "code pop temp" in {
        Coder.code(Pop(Temp, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// pop temp 3
              |@SP
              |A=M
              |A=A-1
              |D=M
              |@8
              |M=D
              |@SP
              |M=M-1
              |""".stripMargin
          )
      }

      "code pop static" in {
        Coder.code(Pop(Static, 3), CoderState(0, ""), "file") shouldEqual
          CoderState(
            0,
            """// pop static 3
              |@SP
              |A=M
              |A=A-1
              |D=M
              |@file.3
              |M=D
              |@SP
              |M=M-1
              |""".stripMargin
          )
      }
    }
  }
}
