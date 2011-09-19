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

object FCA {

  import logging.Logging
  import scala.collection.mutable.{Set, Map}

  object GMDigest {

    import java.security.MessageDigest
    import org.apache.commons.codec.binary.Base64
    
    def digestM (m: Set[Attr]): String = {
      val md = MessageDigest.getInstance ("SHA")
      for (a <- m.toList.sortWith ((a1,a2) => a1 < a2))
        md.update (a.getBytes)
      Base64.encodeBase64URLSafeString (md.digest)
    }
    
    def digestG (g: Set[Obj]): String = {
      val md = MessageDigest.getInstance ("SHA")
      for (a <- g.toList.sortWith ((a1,a2) => a1 < a2))
        md.update (a.getBytes)
      Base64.encodeBase64URLSafeString (md.digest)
    }

  }

  class Concept (val g: Set[Obj], val m: Set[Attr]) {

    import java.security.MessageDigest
    import org.apache.commons.codec.binary.Base64

    val digest: String = {
      val md = MessageDigest.getInstance ("SHA")  
      for (o <- g.toList.sortWith ((g1,g2) => g1 < g2))
        md.update (o.getBytes)
      for (a <- m.toList.sortWith ((a1,a2) => a1 < a2))
        md.update (a.getBytes)
      Base64.encodeBase64URLSafeString (md.digest)
    }

    override def equals (that: Any): Boolean = {
      that match {
        case that: Concept => this.digest == that.digest
        case _ => false
      }
    }

    override def hashCode (): Int = digest.hashCode

    def ident () = digest

    def ≺  (that: Concept) (implicit ctx: Context): Boolean = {
      for (a <- ctx.ᴍOrder) {
        val inThis = this.m contains a
        val inThat = that.m contains a      
        if (!inThis && inThat)
          return true
        else if (inThis && !inThat)
          return false
      }
      false // were equal
    }
  }

  object Concept {

    def apply () = {
      import scala.collection.mutable.HashSet
      new Concept (HashSet.empty, HashSet.empty)
    }

    def apply (g: Set[Obj], m: Set[Attr]) = 
      new Concept (g, m)

    val empty = Concept ()

  }

  /**
   * A Context is a set of objects G, a set of attributes M and their relationship I.
   * Within a Context there is established a lectic sort order allowing for the comparion
   * of two concepts.  If lectically A < B => A ≺ B conceptually.
   * The following obligations are met:
   *  - Set of Objects
   *  - Set of Attributes
   *  - Mapping of G and M to integer values.
   *  - The lectic order of attributes.
   *  - The derive operations G' and M'.
   */
  trait Context {

    def ɢ (): Set[Obj]

    def ᴍ (): Set[Attr]

    def I (): Map[Obj, Set[Attr]]

    def ᴍOrder (): Seq[Attr]

    def ɢComp (g: Iterable[Obj]): Set[Attr]

    def ᴍComp (g: Set[Attr]): Set[Obj]

    def ɢClosure (g: Set[Obj]): Set[Obj]

    def ᴍClosure (m: Set[Attr]): Set[Attr]

    def bottom (): Concept

    //def ≺ (a: Concept, b: Concept): Boolean

  }

  trait ContextLike extends Context {

    def ɢClosure (g: Set[Obj]): Set[Obj] = 
      ᴍComp (ɢComp (g))
    
    def ᴍClosure (m: Set[Attr]): Set[Attr] =
      ɢComp (ᴍComp (m))

  }

  import scala.collection.mutable.{HashMap, HashSet}

  /**
   * Construct a context from a sequence of objects and attributes
   */
  class ContextImpl (g: Set[Obj], m: Set[Attr], i: Map[Obj, Set[Attr]]) extends ContextLike {

    private val _ᴍOrder: Seq[Attr] = m.toList.sorted

    def ᴍOrder () = _ᴍOrder

    def ɢ (): Set[Obj] = g

    def ᴍ (): Set[Attr] = m

    def I (): Map[Obj, Set[Attr]] = i

    def bottom (): Concept = {
      val mTop = ɢComp (Set.empty)
      Concept (ᴍComp (mTop), mTop)
    }

    def ɢComp (gs: Iterable[Obj]): Set[Attr] = {
      var m: HashSet[Attr] = HashSet () ++ (ᴍ)
      for (g <- gs; as <- i get g) {
        m = m intersect as
      }
      m
    }

    def ᴍComp (m: Set[Attr]): Set[Obj] = {
      var g: HashSet[Obj] = HashSet ()
      for ((o, attr) <- i) {
        if (m subsetOf attr)
          g += o
      }
      g
    }

    // def compare (c1: Concept, c2: Concept): Int = {
    //   val mord = mOrder
    
    //   def loop (mord: List[FCASym]): Int =  {
    //     if (mord.isEmpty)
    //       0
    //     else {
    //       val s: FCASym = mord.head
    //       val inC1 = c1.m.contains (s)
    //       val inC2 = c2.m.contains (s)
    //       if (!inC1 && inC2)
    //         -1
    //       else if (inC1 && !inC2)
    //         1
    //       else loop (mord.tail)
    //     }
    //   }

    //   loop (mord)    
    // }

  }


  /**
   * LatticeMorph takes a Context and reduces it.
   * Later given a FCA lattice generated from the reduced Context
   * acts as a homomorphism preserving the lattice structure but
   * populated with the objects from the original Context.
   */
  class LatticeMorph extends Logging {

    private val gs = HashSet[String] ()
    private val is = HashMap[String, Set[String]] ()

    // digest -> set of the original objects that share this attribute hash
    private val omap = HashMap[String, HashSet[String]] ()

    // For now assume all objs have at least one attr.
    // and all attr associated with at least one obj.
    def reduce (ctx: Context): Context = {
      
      for ((o,as) <- ctx.I) {
        val ohash = GMDigest.digestM (as)
        gs += ohash
        is += (ohash -> as)

        val os = omap getOrElse (ohash, HashSet[String] ())
        omap += (ohash -> (os += o))
      }
      new ContextImpl (gs, ctx.ᴍ, is)
    }

    private def expandConcept (c0: Concept): Concept = {
      // fixme memoize this to only remember already expanded Concepts
      val g0 = c0.g
      val g1 = new HashSet[String] ()

      for (g <- g0) {
        omap get g match {
	  case Some (gs) => g1 ++= gs
	  case None => logger.error ("Failed to expand digest to original gs.")
        }
      }
      
      Concept (g1, c0.m)

    }

    def expand (lattice: DListGraph[Concept, Null]): DListGraph[Concept, Null] = {
      val l0 = lattice
      val l1 = new DListLattice[Concept, Null] ()

      // copy over all arcs creating nodes as necessary
      // expanding eash obj digest to is orginal set of objs
      for (a <- l0.arcs) {
        val src = expandConcept (a.source.vertex) // fixme RPR we re-expand a node several times
        val tgt = expandConcept (a.target.vertex)
        l1 + (null, src, tgt)
      }

      l1    
    }
    
  }


}
