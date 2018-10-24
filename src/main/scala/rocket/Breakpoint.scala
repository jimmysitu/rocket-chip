// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
//import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.{CoreModule, CoreBundle}
import freechips.rocketchip.util._

class BPControl(implicit p: Parameters) extends CoreBundle()(p) {
  val ttype = UInt(4.W)
  val dmode = Bool()
  val maskmax = UInt(6.W)
  val reserved = UInt((xLen-24).W)
  val action = Bool()
  val chain = Bool()
  val zero = UInt(2.W)
  val tmatch = UInt(2.W)
  val m = Bool()
  val h = Bool()
  val s = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()

  def tType = 2
  def maskMax = 4
  def enabled(mstatus: MStatus) = !mstatus.debug && Cat(m, h, s, u)(mstatus.prv)
}

class BP(implicit p: Parameters) extends CoreBundle()(p) {
  val control = new BPControl
  val address = UInt(vaddrBits.W)

  def mask(dummy: Int = 0) =
    (0 until control.maskMax-1).scanLeft(control.tmatch(0))((m, i) => m && address(i)).asUInt

  def pow2AddressMatch(x: UInt) =
    (~x | mask()) === (~address | mask())

  def rangeAddressMatch(x: UInt) =
    (x >= address) ^ control.tmatch(0)

  def addressMatch(x: UInt) =
    Mux(control.tmatch(1), rangeAddressMatch(x), pow2AddressMatch(x))
}

class BreakpointUnit(n: Int)(implicit p: Parameters) extends CoreModule()(p) {
  val io = IO(new Bundle {
    val status = Input(new MStatus())
    val bp = Input(Vec(n, new BP))
    val pc = Input(UInt(vaddrBits.W))
    val ea = Input(UInt(vaddrBits.W))
    val xcpt_if = Output(Bool())
    val xcpt_ld = Output(Bool())
    val xcpt_st = Output(Bool())
    val debug_if = Output(Bool())
    val debug_ld = Output(Bool())
    val debug_st = Output(Bool())
  })

  io.xcpt_if := false.B
  io.xcpt_ld := false.B
  io.xcpt_st := false.B
  io.debug_if := false.B
  io.debug_ld := false.B
  io.debug_st := false.B

  io.bp.foldLeft((true.B, true.B, true.B)) { case ((ri, wi, xi), bp) =>
    val en = bp.control.enabled(io.status)
    val r = en && ri && bp.control.r && bp.addressMatch(io.ea)
    val w = en && wi && bp.control.w && bp.addressMatch(io.ea)
    val x = en && xi && bp.control.x && bp.addressMatch(io.pc)
    val end = !bp.control.chain

    when (end && r) { io.xcpt_ld := !bp.control.action; io.debug_ld := bp.control.action }
    when (end && w) { io.xcpt_st := !bp.control.action; io.debug_st := bp.control.action }
    when (end && x) { io.xcpt_if := !bp.control.action; io.debug_if := bp.control.action }

    (end || r, end || w, end || x)
  }
}
