package com.maraney.jackvm

import java.io.FileWriter

import com.maraney.jackvm.Types.SyntaxError

import scala.io.Source
import scala.util.{Failure, Success}

object App {
  def main(args: Array[String]) {
    args.headOption match {
      case None =>
        println(
          "Usage: \"jack-virtual-machine <name>\" will translate <name>.vm to <name>.asm")
      case Some(filename) =>
        val lines = readFile(filename)
        Translator.translate(lines) match {
          case Success(translated) => writeFile(filename, translated)
          case Failure(SyntaxError(line)) =>
            System.err.println("Syntax error on line " + line)
          case Failure(error) =>
            System.err.println("Unrecognised error:" + error)
        }
    }
  }

  private def readFile(filename: String) = {
    Source.fromFile(filename + ".vm").getLines.toList
  }

  private def writeFile(filename: String, translated: String) = {
    val writer = new FileWriter(filename + ".asm")
    writer.write(translated)
    writer.close
  }
}
