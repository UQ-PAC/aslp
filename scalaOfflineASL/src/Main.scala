// package scala

import mainargs.{main, arg, ParserForMethods, Flag}
import ir._
import aslloader.Lifter

object Main{
  @main
  def run(@arg(short = 'o', name="opcode", doc = "Opcode in gcc (little-endian) format")
          opc: String,
          @arg(doc = "Program counter as decimal")
          pcn: String = "0") = {

    val op_num = BigInt(opc.stripPrefix("0x"), 16);

    val pc_num = BigInt(pcn);

    Lifter.liftOpcode(op_num, pc_num).map(x => {
      println(s"${x.label}: ")  
      x.sl.map(st => println(s"   ${st}"))
    })

  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
