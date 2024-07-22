// package scala

package main
import mainargs.{main, arg, ParserForMethods, Flag}

object Main {
  @main
  def run(@arg(short = 'o', name="opcode", doc = "Opcode in gcc (little-endian) format")
          opc: String,
          @arg(doc = "Program counter as decimal")
          pcn: String = "0") = {

    val op_num = BigInt(opc.stripPrefix("0x"), 16);
    val pc_num = BigInt(pcn);

    Lifter.liftOpcode(op_num, pc_num)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
