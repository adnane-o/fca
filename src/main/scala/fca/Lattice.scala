/*
 * FCA graph generator.
 * Copyright (C) 2011 Raymond Racine
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.google.profiles.rayracine
package fca

import logging.Logging
import scala.collection.mutable.{Builder, GrowingBuilder}

/**
 * Canonical builder of Lattices
 * @tparam N the type of node contained in the lattice.
 * @tparam 
 */
class GraphBuilder 

abstract class MutableLatticeFactory[CC[_]]

/**
 * LatticeLike behaviour
 * @tparam V the vertex type
 * @tparam E the edge type
 */
trait Graph[V,E,N,A,R] {

  import scala.collection.generic.CanBuildFrom

  /**
   * Number of arcs going out from the given vertex.
   * @param v the vertex.
   */
  def outDegree (n: N): Int

  /**
   * Number of arcs coming into the vertex v.
   */
  def inDegree (n: N): Int

  /**
   * Source vertex for the arc e.
   */
  def source (a: A): N

  /**
   * Target vertex of the arc e.
   */
  def target (a: A): N

  /**
   * target(e) if vertex v is the source of arc e, otherwise source (e)
   * @param the vertex
   * @param the edge
   * @returns source or target vertex.
   */
  def opposite (n: N, a: A): N

  /**
   * The order of the graph, i.e., number of nodes.
   */
  def order (): Int

  /**
   * The size of the graph, i.e., the number of edges.
   */

  def size (): Int

  /**
   * A random vertex of the graph.
   * @returns Some[V] or None if graph is empty.
   */
  // def chooseNode (): Option[V]

  // def chooseEdge(): Option[E]

  def allNodes (): Seq[N]

  def allArcs (): Seq[A]

  def inArcs (n: N): Seq[A]

  def outArcs (n: N): Seq[A]

  /**
   * Add new node
   */
  def + (v: V): R

  /**
   * Add a new edge
   */
  def + (e: E, src: V, tgt: V): R

  //def delEdge (e: E): R 

  /**
   * Delete vertex v from the graph along with all associated inbound or outbound arcs.
   */
  //def - (v: V): R

  /**
   * Deletes arc e from the graph.
   */
  // def removeEdge[F >: E, S] (e: F) (implicit bf: CanBuildFrom[R,F,S]): S

  // def sortNodes[W >: V, S] (cmp: Comparable[W]) (implicit bf: CanBuildFrom[R,W,S]): S

  // def sortEdges[W >: V, S] (cmp: Comparable[E]) (implicit bs: CanBuildFrom[R,W,S]): S

  def isEmpty(): Boolean

}

/**
 * Two nodes are equal if and only if they have equal payloads
 */
private[fca] class Node[V,E] (val vertex: V, var inArcs : DList[Arc[V,E]], var outArcs: DList[Arc[V,E]]) {
  override def equals (that: Any) = {
    that match {
        case that: Node[_,_] => that.vertex == this.vertex
        case _ => false
    }
  }
  override def hashCode (): Int = {
    vertex.hashCode
  }
}

/**
 * Two Arcs are equal if they have the same src and tgt nodes. AND EDGE RPR
 */
private[fca] class Arc[V,E] (val edge: E, val source: Node[V,E], val target: Node[V,E], 
                             var sameSource: DList[Arc[V,E]], var sameTarget: DList[Arc[V,E]]) {
  override def equals (that: Any) = {
    that match {
	case that: Arc[_,_] => this.source == that.source && this.target == that.target // && this.edge == that.edge
	case _ => false
    }
  }

  override def hashCode (): Int = 
    source.hashCode ^ target.hashCode // ^ edge.hashCode

}

import scala.collection.mutable.Set

class DListGraph[V,E] ()
extends Graph[V,E,Node[V,E], Arc[V,E], DListGraph[V,E]] () with Logging {
  
  import scala.collection.mutable.HashMap
  import scala.collection.mutable.HashSet
  
  protected[fca] var nodes = DList.empty[Node[V,E]]
  protected[fca] var arcs = DList.empty[Arc[V,E]]
  
  // note asymmetry here.  Nodes are unique by payload, Arcs by src/tgt/edge.
  private var nodeSet = HashMap.empty[V,Node[V,E]]
  private var arcSet  = HashSet.empty[Arc[V,E]]
  
  def apply (v: V): Option[Node[V,E]] = 
    nodeSet get v
  
  def order () = nodeSet.size
  
  def size () = arcs.size
  
  private def addNode (v: V): Node[V,E] = {
    val node = new Node[V,E] (v, DList.empty[Arc[V,E]], DList.empty[Arc[V,E]])
    nodes = nodes :+ node
    nodeSet += ((v, node))
    node
  }
  
  def + (v: V): DListGraph[V,E] = {
    if (! (nodeSet contains v))
      addNode (v)
    this
  }
  
  def contains (v: V): Boolean = {
    nodeSet contains v
  }
  
  def lookup (v: V): Option[Node[V,E]] =
    nodeSet get v
  
  /**
   * Create an arc with value E between the src and tgt vertexs.
   * Create the nodes if necessary.
   */
  def + (e: E, src: V, tgt: V): DListGraph[V,E] = {
    val srcN = nodeSet.getOrElseUpdate (src, addNode (src))
    val tgtN = nodeSet.getOrElseUpdate (tgt, addNode (tgt))
    val arc = new Arc[V,E] (e, srcN, tgtN, srcN.outArcs, tgtN.inArcs)
    if (! (arcSet contains arc)) {
      srcN.outArcs = srcN.outArcs :+ arc
      tgtN.inArcs = tgtN.inArcs :+ arc
      arcs = arcs :+ arc
      arcSet += arc
    }
    this
  }
  
  private def delArcs (as: DList[Arc[V,E]]): Unit = 
    as foreach (delEdge (_))

  def delNode (node: Node[V,E]): DListGraph[V,E] = {
    delArcs (node.inArcs)
    delArcs (node.outArcs)
    nodes filterNot (_ == node)
    this
  }

  /**
   * Remove an arc from source and target nodes.
   */
  def delEdge (arc: Arc[V,E]): DListGraph[V,E] = {
    // for (aDL <- seekArc (arc)) {
    //   aDL.remove
    //   arc.source.outArcs filterNot (_ == arc)
    //   arc.target.inArcs  filterNot (_ == arc)
    //   arc.sameSource foreach (_.sameSource filterNot (_ == arc))
    //   arc.sameTarget foreach (_.sameTarget filterNot (_ == arc))
    //   arcSet -= arc
    // }
    this
  }
    
    
  def - (v: Node[V,E]): DListGraph[V,E] = {

    // def del (dl: DList[Node[V,E]]): Unit =
    //   if (dl.isEmpty) {}
    //   else if (v == dl.head)
    //     dl.remove         
    //   else del (dl.tail)
    
    // del (nodes)
    this
  }

  def inArcs (n: Node[V,E]) = 
    n.inArcs
  
  def outArcs (n: Node[V,E]) =
    n.outArcs

  def inDegree (n: Node[V,E]) =
    n.inArcs.size
  
  def outDegree (n: Node[V,E]) =
    n.outArcs.size

  def opposite (v: Node[V,E], a: Arc[V,E]) = 
    if (a.source == v)
      a.target
    else a.source
  
  def source (a: Arc[V,E]): Node[V,E] =
    a.source

  def target (a: Arc[V,E]): Node[V,E] =
    a.target
  
  def allNodes () = nodes

  def allArcs () = arcs

  def isEmpty () =
    nodes.isEmpty && arcs.isEmpty  

}

object DListGraph {
  def empty[V,E]() = new DListGraph[V,E] ()
}

object DListGraphUtils {
/**
 * Dump the graph to stdout.
 */
  def rawDump[V,E] (g: DListGraph[V,E]): Unit = {
    println ("Raw Dump of " + g.toString)
    val ns = g.allNodes
    for (n <- ns) {
      println ("NODE: " + n.toString + "::" + n.vertex)
      println (" In Arcs:")
      for (a <- n.inArcs) {
        println ("  Arc: " + a.toString)
      }
      println (" Out Arcs: " )
      for (a <- n.outArcs) {
        println ("  Arc: " + a.toString)
      }
    }

    for (a <- g.allArcs) {
      println ("ARC: " + a.toString)
      println (" Edge: " + (if (a.edge == null) "Null" else a.edge))
      println (" Source: " + a.source.toString + "::" + a.source.vertex)
      println (" Target: " + a.target.toString + "::" + a.target.vertex)
      println (" Same Source: ") 
      for (as <- a.sameSource) {
        println ("  Arc: " + as.toString)
      }
      println (" Same Target: " ) 
      for (at <- a.sameTarget) {
        println ("  Arc: " + at.toString)
      }      
    }
  }

  def dotAttrFile (g: DListGraph[FCA.Concept, Null]): Unit = {
    def nodeName (c: FCA.Concept): String = {
      val attrS = c.m.toList.sortWith ((s1, s2) => s1 < s2).foldLeft ("") ((s, a) => s + a)
      attrS
    }
    
    import java.io.PrintStream
    val out = new PrintStream ("latticeAttr.dot")

    out.println ("digraph Attrs  {")
    
    for (a <- g.allArcs) {
      out.println ("  " + nodeName (a.target.vertex) + " -> " + nodeName (a.source.vertex) + ";")
    }

    out.println ("}")
    out.close
  }  
  
  def dotFile (g: DListGraph[FCA.Concept,Null]): Unit = {

    def nodeName (c: FCA.Concept): String = {
      val objS = c.g.toList.sortWith ((s1, s2) => s1 < s2).foldLeft ("") ((s, n) => s + n)
      val attrS = c.m.toList.sortWith ((s1, s2) => s1 < s2).foldLeft ("") ((s, a) => s + a)
      attrS + objS
    }
    
    import java.io.PrintStream
    val out = new PrintStream ("lattice.dot")
    out.println ("digraph lattice {")
    for (a <- g.allArcs) {
      out.println (nodeName (a.target.vertex) + " -> " + nodeName (a.source.vertex) + ";")
    }
    out.println ("}")
    out.close
  }    
}

trait Lattice[V,E] extends DListGraph [V,E]  {  
  def top (): Option[Node[V,E]]
}


class DListLattice[V,E] extends DListGraph[V,E] with Lattice[V,E]{
  def top (): Option[Node[V,E]] = {
    
    def walkUp (n: Node[V,E]): Node[V,E] = {
      if (n.outArcs.isEmpty)
	return n
      else walkUp (n.outArcs.head.target)
    }
    
    if (allNodes.isEmpty) None
    else Some (walkUp (allNodes.head))
  }
}
