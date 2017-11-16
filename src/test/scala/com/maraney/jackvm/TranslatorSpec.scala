package com.maraney.jackvm

import scala.io.Source
import scala.util.{Failure, Success}

class TranslatorSpec extends UnitSpec {
  "The Translator" must {
    "translate BasicSpec" in {
      val input =
        Source.fromFile("src/test/fixtures/SimpleAdd.vm").getLines.toList
      val desiredOutput =
        Source.fromFile("src/test/fixtures/SimpleAdd.asm").mkString("")
      val output = Translator.translate(input)

      output.get shouldEqual desiredOutput
    }

    "translate StackTest" in {
      val input =
        Source.fromFile("src/test/fixtures/StackTest.vm").getLines.toList
      val desiredOutput =
        Source.fromFile("src/test/fixtures/StackTest.asm").mkString("")
      val output = Translator.translate(input)

      output.get shouldEqual desiredOutput
    }

  }
}
