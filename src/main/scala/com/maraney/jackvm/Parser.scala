package com.maraney.jackvm

import com.maraney.jackvm.Types._

object Parser {
  def parse(line: String) = {

    val stripWhiteSpaceAndComments = raw"\s*(([\w ]*\w)|())\s*(//.*)?".r
    val pushCmd = raw"push ([a-z]*) (\d{1,5})".r

    line match {
      case stripWhiteSpaceAndComments(command, _, _, _) =>
        command match {
          case pushCmd(segment, value) =>
            segment match {
              case "constant" => Push(Constant, value.toShort)
              case "static"   => Push(Static, value.toShort)
              case "this"     => Push(This, value.toShort)
              case "local"    => Push(Local, value.toShort)
              case "argument" => Push(Argument, value.toShort)
              case "that"     => Push(That, value.toShort)
              case "pointer"  => Push(Pointer, value.toShort)
              case "temp"     => Push(Temp, value.toShort)
              case _          => Malformed
            }
          case "add" => Add
          case "sub" => Sub
          case "neg" => Neg
          case "eq"  => Eq
          case "gt"  => Gt
          case "lt"  => Lt
          case "and" => And
          case "or"  => Or
          case "not" => Not
          case ""    => NoOp
          case _     => Malformed
        }
      case _ => Malformed
    }
  }
}
