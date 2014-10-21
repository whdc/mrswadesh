package mrswadesh

import collection.mutable.ArrayBuffer

object Tree extends App {

  val inf = Double.PositiveInfinity

  val clades = Clades.read("data/clades.txt")

  // clade constraint, optional TMRCA constraint
  case class Clade(
    name:String,
    lb:Double,
    ub:Double
  ) {}

  case class Node(
    id:Int,
    var clade:Clade,  // clade constraint
    var t:Double,
    var up:Node = null, 
    var lc:Node = null, 
    var rc:Node = null
  ) {
    def prefix: IndexedSeq[Node] = IndexedSeq(this) ++
      (if(lc == null) IndexedSeq.empty[Node] else lc.prefix) ++
      (if(rc == null) IndexedSeq.empty[Node] else rc.prefix)

    def sib = if (up.lc == this) up.rc else up.lc
    def ups : IndexedSeq[Node] = this +: 
      (if(up == null) IndexedSeq.empty[Node] else up.ups)

    def lb = if (clade == null) -inf else clade.lb
    def ub = if (clade == null) inf else clade.ub
    def name = if (clade == null) "" else clade.name

    // disconnect parent, connect sib with gp
    def prune() {
      val s = sib
      val gp = up.up

      s.up = gp
      if (gp.lc == up) gp.lc = s else gp.rc = s
      up.up = null
      if (up.lc == this) up.rc = null else up.lc = null
    }

    // graft noo above this; noo must have no more than one child
    def graft(noo:Node) {  
      if (noo.lc == null) noo.lc = this else noo.rc = this
      noo.up = up
      if (up.lc == this) up.lc = noo else up.rc = noo
      up = noo
    }
  }

  object Node {
    val id_pez = Iterator.iterate(0)(_ + 1)

    def xlt( src: Clades.Node) = {  // translate from Clades.Node
      val (lb, ub, t) = src.prior_ match {
        case Clades.PointPrior( s) +: _ => (s, s, s)
        case Clades.UniformPrior( l, u) +: _ => (l, u, 0.5*(l+u))
        case _ => (-inf, inf, 0.0)
      }

      Node(id_pez.next, Clade(src.name, lb, ub), t)
    }

    def hroot() = Node(id_pez.next, Clade("hroot", inf, inf), inf)

    def split() = Node( id_pez.next, null, 0.0)
  }

  val hroot = Node.hroot()

  // skip nodes of out-degree 1 if they are not attested languages
  def skipMereLabel(x: Clades.Node) : Clades.Node = {
    if (x.ch_.length == 1 && (x.flags contains '#')) {
      skipMereLabel(x.ch_(0))
    } else {
      x
    }
  }

  // copy srcs and make them children of dst;
  // binarize if necessary
  def copytree( srcs: IndexedSeq[Clades.Node], dst: Node) {
    if (srcs.isEmpty) return

    val ch = skipMereLabel(srcs(0))
    val noo = Node.xlt(ch)
    noo.up = dst
    dst.lc = noo
    copytree(ch.ch_, noo)

    if (srcs.length == 1) return

    if (srcs.length == 2) {
      val ch = skipMereLabel(srcs(1))
      val noo = Node.xlt(ch)
      noo.up = dst
      dst.rc = noo
      copytree(ch.ch_, noo)
    } else {
      val noo = Node.split()
      noo.up = dst
      dst.rc = noo
      copytree(srcs.tail, noo)
    }
  }

  copytree(IndexedSeq(clades.tree), hroot)

  val nodes = hroot.prefix.sortBy(_.id)

  // simulate heat equation to initialize node times
  for (i <- 0 until 100) {
    for (n <- hroot.lc.prefix) {
      n.t = if (n.lb == n.ub) n.lb else {
        val lb = Seq(0.0, n.lb, 
          if (n.lc == null) 0.0 else n.lc.t,
          if (n.rc == null) 0.0 else n.rc.t).max
        val ub = Seq(
          if (n.up == hroot) lb + 1000.0 else n.up.t,
          n.ub).min
        0.5 * (lb + ub)
      }
    }
  }

  object Color {
    val RED = "\033[91m"
    val GRN = "\033[92m"
    val BLU = "\033[94m"
    val END = "\033[0m"
  }

  def ptree() {
    for (n <- hroot.lc.prefix) {
      val guide = (for (i <- n.ups.reverse.tail) yield {
        if (i == n && n.lc == null && n.sib != null && n.up.lc == n) "├──"
        else if (i == n && n.lc == null) "└──"
        else if (i == n && n.sib != null && n.up.lc == n) "├─┐"
        else if (i == n) "└─┐"
        else if (i.up.rc == null) "  "
        else if (i.up.rc == i) "  "
        else "│ "
      }).mkString("")
      println(guide + Color.BLU + n.id + " " + Color.RED + n.name + " " + 
        Color.GRN + n.t.toInt + Color.END)
    }
  }

  // Return set of points on tree that no more than a distance r from 
  // (i,t), and with time greater than lb.  Stop at clade constraints.
  def ball(i:Node, t:Double, r:Double, lb:Double, ub:Double, mrca:Node) = {
    // println( "  -- %d %f %f %f".format(i, t.floor, r.floor, lb.floor))
    val segments = ArrayBuffer.empty[(Node,Double,Double)]
    
    def ball_down(i:Node, t:Double, r:Double, lb:Double, mrca:Node) {
      if (i == null) return

      if (t - r > lb && t - r > i.t) {
        segments.append((i, t-r, t))
      } else if (lb > i.t) {
        segments.append((i, lb, t))
      } else {
        segments.append((i, i.t, t))
        if (i.clade == null) {
          ball_down(i.lc, i.t, r-(t-i.t), lb, mrca)
          ball_down(i.rc, i.t, r-(t-i.t), lb, mrca)
        }
      }
    }

    def ball_up(i:Node, t:Double, r:Double, lb:Double, ub:Double, mrca:Node) {
      val tup = i.up.t

      if (t + r < tup && t + r < ub) {
        segments.append((i, t, t+r))
      } else if (ub < tup) {
        segments.append((i, t, ub))
      } else {
        segments.append((i, t, tup))
        if (i != mrca) {
          ball_down(i.sib, tup, r-(tup-t), lb, mrca)
          ball_up(i.up, tup, r-(tup-t), lb, ub, mrca)
        }
      }
    }

    ball_down(i, t, r, lb, mrca) 
    ball_up(i, t, r, lb, ub, mrca)

    segments
  }

  // draw UAR from tree segments
  def draw( segments: ArrayBuffer[(Node,Double,Double)]) : (Node,Double) = {
    val n_seg = segments.length

    var i_seg = 0
    var s = 0.0
    while (i_seg < n_seg) {
      val (i, t0, t1) = segments(i_seg)
      s += t1-t0
      i_seg += 1
    }

    var x = s * util.Random.nextDouble()

    i_seg = 0
    while (i_seg < n_seg) {
      val (i, t0, t1) = segments(i_seg)
      if (x <= t1-t0) {
        return (i, x + t0)
      }
      x -= t1-t0
      i_seg += 1
    }

    return null
  }

  def spr(i:Node) {
    assert(i.up != null && i.sib != null)  // must have sibling

    //println( "spr on %d %s".format(i.id, i.name))

    val sib = i.sib

    // get mrca
    val has_clade = i.up.ups.find(_.clade != null).get
    val mrca = if (has_clade == i.up) sib else has_clade 
    val clade = has_clade.clade

    // if mrca satisfies lb, then graft does not have to
    val lb = if (mrca.t >= clade.lb) i.t else i.t.max(clade.lb)
    if (lb == clade.ub) return
    // that was the last change to back out

    // start manipulating
    has_clade.clade = null
    i.prune() // remove parent of i from tree

    val (i_new, t_new) = draw(ball(sib, i.up.t, 1000.0, lb, clade.ub, mrca))

    //println( "graft at %d %s %f".format(i_new.id, i_new.name, t_new.floor))

    // reconfigure u
    i.up.t = t_new
    i_new.graft( i.up)
    if (mrca == i_new) {
      i.up.clade = clade
    } else {
      mrca.clade = clade
    }
  }

  def slide(i:Node) {
    val ub = i.ub min i.up.t
    val lb = i.lb max 
      (if(i.lc == null) -inf else i.lc.t) max 
      (if(i.rc == null) -inf else i.rc.t)
    if (lb==ub) return
    i.t = lb + util.Random.nextDouble() * (ub-lb)
  }
  
  val spr_nodes = nodes.filter(i => i.up != null && i.sib != null)
  val slide_nodes = nodes.filter(i => i.up != null && i.ub > i.lb)

  val fo = new java.io.FileWriter("data/root.txt")
  ptree()
  for (iter <- 0 until 1000000) {
    spr(spr_nodes(util.Random.nextInt( spr_nodes.length)))
    slide(slide_nodes(util.Random.nextInt( slide_nodes.length)))
    if (iter % 100000 == 0) ptree()
    if (iter > 100000 && iter % 1000 == 0) fo.write("%f\n".format(hroot.lc.t))
  }
}
