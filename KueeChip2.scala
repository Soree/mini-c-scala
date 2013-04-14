// kueechip2 simulator
// (C) Akira Hayakawa, 2013

import scala.collection.mutable._

class State {
  type Flag = (Boolean, Boolean, Boolean, Boolean)

  // -128 ~ +127
  // TODO unit test
  class Bits(_v: Int){
    val v = _v
    val data = new ArrayBuffer[Int]()
    for(i <- 0 until 8){
      data += 0
    }

    if(v < 0){
      fill( (-1) * v - 1 )
      flipAll
    }else{
      fill(v)
    }

    def fill(v: Int){
      for(i <- 0 until 8){
        if((v & (1 << i)) == 1){
          data(i) = 1
        }
      }
    }

    def flipAll(){
      for(i <- 0 until 8){
        if(data(i) == 1){
          data(i) = 0
        }else{
          data(i) = 1
        }
      }
    }

    def compute_carry(o: Bits): Boolean = {
      var r: Boolean = false

      var c: Int = 0
      for(i <- 0 until 8){
        val x = data(i)
        val y = o.data(i)
        if(x+y+c > 1){
          c = 1
        }else{
          c = 0
        }
      }

      if(c == 1){
        r = true
      }
      return r
    }
    
    def add(o: Bits): (Bits, Flag) = {
      var cf, nf, vf, zf: Boolean = false
      val r = v + o.v
      if(r > 127){
        vf = true
      }
      if(r < 0){
        nf = true
      }
      if(r == 0){
        zf = true
      }

      cf = compute_carry(o)

      return (new Bits(r), (cf, nf, vf, zf))
    }

    def inverse(): Bits = {
      return new Bits( (-1) * v )
    }
    
    def sub(o: Bits): (Bits, Flag) = {
      var cf, nf, vf, zf: Boolean = false
      val r = v - o.v
      if(r < -128){
        vf = true
      }
      if(r < 0){
        nf = true
      }
      if(r == 0){
        zf = true
      }

      cf = compute_carry(o.inverse())

      return (new Bits(r), (cf, nf, vf, zf))
    }
  }

  val mem = new ArrayBuffer[Int]()
  for(i <- 0 until 512){
    mem += 0
  }

  val reg = new ArrayBuffer[Int]()
  for(i <- 0 until 2){
    reg += 0
  }

  type B = (Int, Int, Boolean)
  val ACC = 0
  val IX = 1
  val IM = 2
  val OTHER = 3

  val MEM_P = 0
  val MEM_D = 1

  reg(ACC) = 0
  reg(IX) = 0

  var cf: Int = 0
  var nf: Int = 0
  var vf: Int = 0
  var zf: Int = 0

  var pc: Int = 0

  def show(): Unit = {
  }

  def nop() = {}
  def halt() = {}
  def rcp = { cf = 0 }
  def scf = { cf = 1 }

  def addr_mem(mem_type:Int, v:Int): Int = {
    v << mem_type  
  }

  def reg_if(p: Boolean): Int = {
    var r = 0
    if(p){
      r += reg(IX)   
    }
    return r
  }

  
  def st(a: Int, b: Int, btype: B): Unit = {
    val (base, mem_type, ix) = btype

    if(base == IM){
      mem(b) = reg(a)
      return
    }

    if(base < IM){
      reg(base) = reg(a)
      return
    }

    val x = b + reg_if(ix)
    val y = addr_mem(mem_type, x)
    mem(y) = reg(a)
  }

  def data_of(b: Int, btype: B): Int = {
    val (base, mem_type, ix) = btype

    if(base == IM){
      return b
    }

    if(base < IM){
      return reg(base)
    }

    val x = b + reg_if(ix) 
    val y = addr_mem(mem_type, x)
    mem(y)
  }

  def ld(a: Int, b: Int, btype: B): Unit = {
    reg(a) = data_of(b, btype)
  }

  def sbc(a: Int, b: Int, btype: B): Unit = {
    val x = reg(a)
    val y = data_of(b, btype)
    val z = cf

    val (r1, (c1, _, v1, _)) = new Bits(x).sub(new Bits(y))
    val (r2, (c2, n2, v2, z2)) = r1.sub(new Bits(z))

    if(c1 || c2){
      cf = 1
    }
    if(n2){
      nf = 1
    }
    if(v1 || v2){
      vf = 1
    }
    if(z2){
      zf = 1
    }
  }

  def adc(a: Int, b: Int, btype: B): Unit = {
  }

  def sub(a: Int, b: Int, btype: B): Unit = {
  }

  def add(a: Int, b: Int, btype: B): Unit = {
  }
}
