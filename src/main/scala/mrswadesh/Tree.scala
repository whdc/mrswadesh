package mrswadesh

import collection.mutable.ArrayBuffer

object Tree extends App {

  val n_node = 10  // including node at infinity

  val up = Array.tabulate(n_node)((i:Int) => 
    if (i==0) -1 
    else if (i==1) 0
    else i-(i%2)-1)
  val lc = Array.tabulate(n_node)((i:Int) => 
    if (i==0) 1
    else if (i == n_node-1) -1
    else if (i%2==1) i+1 
    else -1)
  val rc = Array.tabulate(n_node)((i:Int) => 
    if (i==0) -1
    else if (i == n_node-1) -1
    else if (i%2==1) i+2 
    else -1)

  def sib(i:Int) = {
    val l = lc(up(i))
    val r = rc(up(i))
    if (l==i) r else l
  }

  def replace_node(old_node:Int, new_node:Int) {
    val u = up(old_node)
    if (lc(u) == old_node) lc(u) = new_node else rc(u) = new_node
    up(new_node) = u
  }

  // insert y above x; does not configure y
  def insert_node(x:Int, y:Int) {  
    if (lc(up(x)) == x) lc(up(x)) = y else rc(up(x)) = y
    up(x) = y
  }

  val time = Array.tabulate(n_node)((i:Int) => 
    if (i==0) Double.PositiveInfinity
    else 5000.0 / (n_node-1) * (n_node-1-i))

  object Color {
    val RED = "\033[91m"
    val END = "\033[0m"
  }

  def ptree(i:Int):String = {
    "(" + Color.RED + i + Color.END + ":" + time(i).floor +
    (if (lc(i) >= 0) (" " + ptree(lc(i))) else "") +
    (if (rc(i) >= 0) (" " + ptree(rc(i))) else "") + ")"
  }

  // return set of points on tree that no more than a distance r from 
  // (i,t), and with time greater than lb
  def ball(i:Int, t:Double, r:Double, lb:Double) = {
    println( "  -- %d %f %f %f".format(i, t.floor, r.floor, lb.floor))
    val segments = ArrayBuffer.empty[(Int,Double,Double)]
    
    def ball_down(i:Int, t:Double, r:Double, lb:Double) {
      assert( segments.length < 20)
      println( "  dn %d %f %f %f".format(i, t.floor, r.floor, lb.floor))
      if (i == -1) return

      if (t - r > lb && t - r > time(i)) {
        segments.append((i, t-r, t))
      } else if (lb > time(i)) {
        segments.append((i, lb, t))
      } else {
        segments.append((i, time(i), t))
        ball_down(lc(i), time(i), r-(t-time(i)), lb)
        ball_down(rc(i), time(i), r-(t-time(i)), lb)
      }
    }

    def ball_up(i:Int, t:Double, r:Double, lb:Double) {
      assert( segments.length < 20)
      println( "  up %d %f %f %f".format(i, t.floor, r.floor, lb.floor))
      val tup = time(up(i))

      if (tup - t > r) {
        segments.append((i, t, t+r))
      } else {
        segments.append((i, t, tup))
        ball_down(sib(i), tup, r-(tup-t), lb)
        ball_up(up(i), tup, r-(tup-t), lb)
      }
    }

    ball_down(i, t, r, lb) 
    ball_up(i, t, r, lb)

    segments
  }

  // draw UAR from tree segments
  def draw( segments: ArrayBuffer[(Int,Double,Double)]) : (Int,Double) = {
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

  def spr(i:Int) {
    assert(i != 0 && i != lc(0))

    println( "spr on " + i)

    // remove parent of i from tree
    val u = up(i) 
    val s = sib(i)
    replace_node(u, s)

    println( "removing " + u)
    println(ptree(lc(0)))

    val (i_new, t_new) = draw(ball(s, time(u), 1000.0, time(i)))

    println( "attach at %d %f".format(i_new, t_new.floor))

    // reconfigure u
    time(u) = t_new
    if (lc(u) == i) rc(u) = i_new else lc(u) = i_new
    up(u) = up(i_new)

    insert_node(i_new,u)
  }
  
  println(ptree(lc(0)))
  for (iter <- 0 until 100) {

    // draw from 1 to n_node-1, excluding lc(0)
    var i = util.Random.nextInt( n_node - 2) + 1
    if (i >= lc(0)) i += 1
    spr(i)

    println(ptree(lc(0)))
  }
}
