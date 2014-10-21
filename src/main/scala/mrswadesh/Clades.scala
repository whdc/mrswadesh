package mrswadesh

import scala.language.dynamics
import scala.math._

case class Clades( tree: Clades.Node) {}

object Clades {

  class Prior;
  case class PointPrior( time: Double) extends Prior;
  case class UniformPrior( lower: Double, upper: Double) extends Prior;

  case class Node( name: String, flags: String, alias_ : Seq[String],
    prior_ : Seq[Prior], ch_ : IndexedSeq[Node]) extends Dynamic
  {
    var up:Option[Node] = None  // parent

    val dict = scala.collection.mutable.HashMap.empty[String, Any]
    def selectDynamic( key: String) = dict.getOrElse( key, null)
    def updateDynamic( key: String)(v: Any) { dict( key) = v }

    def infix: IndexedSeq[Node] = (ch_.take(1).map( _.infix).flatten :+ this) ++
      ch_.drop(1).map( _.infix).flatten
    def prefix: IndexedSeq[Node] = this +: ch_.map( _.prefix).flatten
    def postfix: IndexedSeq[Node] = ch_.map( _.postfix).flatten :+ this
    def isLeaf = ch_.isEmpty
    def uplink( n: Option[Node]) {
      up = n
      for( c <- ch_) {
        c.uplink( Some(this))
      }
    }

    // ancestors, including self
    def up_ : IndexedSeq[Node] = this +: up.fold( IndexedSeq.empty[Node])( _.up_)
  }

  def read( fn: String) = {
    var iAncestor = 0

    val nil = List.empty[Node]
    val clades =
      io.Source.fromFile( fn).mkString.trim
        .split( """\n""").filterNot( _.matches( """\s*""")).toIterator.buffered

    // calculate indentation
    def indent( s: String) = s.indexWhere( _.toString.matches("""\S"""))

    def readNode(): Node = {
      val s = clades.next()
      val (left, right) =
        if( s contains ":") s.splitAt( s.indexOf(":"))
        else (s, "")
      val P = """([#+*^]*)([\S]+)\s*(\[([^\[\]]+)\])?""".r
      val (name, flags, aliasList) = left.trim match {
        case P(x,y,_,z) => (y, x, z)
      }
      val ch_ = readChildren( indent( s))
      val prior_ : Seq[Prior] = {
        val p_ = for( f <- right.split(":").tail) yield {
          val h_ = f.drop(1).trim.split("""\s+""").toSeq
          h_(0) match {
            case "uniform" => UniformPrior( h_(1).toDouble, h_(2).toDouble)
            case _ => PointPrior( h_(0).toDouble)
          }
        }
        if( ch_.isEmpty && p_.isEmpty) Seq( PointPrior(0.0)) else p_
      }
      val alias_ : Seq[String] = if( aliasList==null) Seq.empty[String]
        else aliasList.split( ",").map( _.trim)
      Node( name, flags, alias_, prior_, ch_)
    }

    def readChildren( j: Int): IndexedSeq[Node] = {
      if( !clades.hasNext) return IndexedSeq.empty[Node]
      var k = indent( clades.head)
      if( k <= j) return IndexedSeq.empty[Node]
      readNode() +: readChildren( j)
    }

    val tree = readNode()
    tree.uplink( None)  // make uplinks

    Clades( tree)
  }
}
