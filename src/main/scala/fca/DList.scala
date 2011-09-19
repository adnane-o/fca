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

import scala.collection.Seq

object DList {
  def empty[T] () = 
    new DList[T] ()
}

import java.util.LinkedList

class DList[T] private (l: LinkedList[T]) extends Seq[T] {

  def this () = 
    this (new LinkedList[T] ())

  def apply (idx: Int) =
    l.get (idx)

  override def size (): Int = 
    l.size

  def length (): Int = 
    l.size

  override def head (): T = l.getFirst

  //def tail (): T = l.getLast

  override def isEmpty(): Boolean =
    l.isEmpty

  override def toSeq (): Seq[T] =
    this

  def :+ (e: T): DList[T] = {
    l.add (e)
    this
  }

  def iterator (): Iterator[T] = {
    new Iterator[T]() {
      val iter = l.iterator

      def hasNext () =
	iter.hasNext

      def next () = 
	iter.next
    }
  }
    

  def map[U] (f: T => U): DList[U] = {
    import java.util.ListIterator
    val nl = new LinkedList[U] ()
    val iter = l.listIterator (0)
    while (iter.hasNext) {
      val e = iter.next
      nl.add (f(e))
    }
    new DList[U] (nl)
  }

}


