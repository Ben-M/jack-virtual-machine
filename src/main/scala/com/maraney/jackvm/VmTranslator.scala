package com.maraney.jackvm

import java.io.FileWriter

import com.maraney.jackvm.Types.SyntaxError

import scala.io.Source
import scala.util.{Failure, Success}

object VmTranslator {
  def main(args: Array[String]) {
    args.headOption match {
      case None => echoUsage
      case Some(path) =>
        val nameAndPath = raw"\A(.*\/(\w*))\.vm\z".r
        path match {
          case nameAndPath(path, fileName) =>
            val lines = readFile(path + ".vm")
            println("Parsing: " + path)
            Translator.translate(lines, fileName) match {
              case Success(translated) => writeFile(path + ".asm", translated)
              case Failure(SyntaxError(line)) =>
                System.err.println("Syntax error on line " + line)
              case Failure(error) =>
                System.err.println("Unrecognised error:" + error)
            }
          case _ => echoUsage
        }
    }
  }

  private def readFile(filename: String) = {
    Source.fromFile(filename).getLines.toList
  }

  private def writeFile(filename: String, translated: String) = {
    val writer = new FileWriter(filename)
    writer.write(translated)
    writer.close
  }

  private def echoUsage =
    println(
      "Usage: \"jack-virtual-machine <name>.vm\" will translate <name>.vm to <name>.asm")
}
