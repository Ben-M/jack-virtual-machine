package io.bigpanda.sherlock.command.model.command.json

import com.maraney.jackvm.Coder.CoderState
import com.maraney.jackvm.Types._
import com.maraney.jackvm.{Coder, UnitSpec}

class CoderSpec extends UnitSpec {
  "The Coder" when {
    "coding arithmetic instructions" must {
      "code add" in {
        Coder.code(Add, CoderState(0, "")) shouldEqual
          CoderState(0,
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
        Coder.code(Sub, CoderState(0, "")) shouldEqual
          CoderState(0,
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
        Coder.code(Neg, CoderState(0, "")) shouldEqual
          CoderState(0,
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
        Coder.code(Eq, CoderState(0, "")) shouldEqual
          CoderState(1,
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
              |""".stripMargin)
      }
      "code gt and increment comparison count" in {
        Coder.code(Gt, CoderState(0, "")) shouldEqual
          CoderState(1,
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
              |""".stripMargin)
      }
      "code lt and increment comparison count" in {
        Coder.code(Lt, CoderState(0, "")) shouldEqual
          CoderState(1,
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
              |""".stripMargin)
      }

      "code and" in {
        Coder.code(And, CoderState(0, "")) shouldEqual
          CoderState(0,
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
        Coder.code(Or, CoderState(0, "")) shouldEqual
          CoderState(0,
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
        Coder.code(Not, CoderState(0, "")) shouldEqual
          CoderState(0,
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
        Coder.code(Push(Constant, 3), CoderState(0, "")) shouldEqual
          CoderState(0,
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
    }
  }
}