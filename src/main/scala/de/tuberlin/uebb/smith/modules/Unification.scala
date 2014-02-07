/*
 * Copyright (c) 2012, Martin Zuber
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials provided
 *   with the distribution.
 * - Neither the name of the TU Berlin nor the names of its
 *   contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package de.tuberlin.uebb.smith.modules

import scala.collection.mutable._
import org.kiama.rewriting.Rewriter._
import scalaz._
import Scalaz._
import TypeSynonyms._
import Syntax._


/**
  * Adapted version of 
  * https://github.com/mzuber/typechecklib/blob/master/core/src/main/scala/typechecklib/Unification.scala
  */
object Unification {

  /**
    * Convenience for DefaultUnification.unify
    */
  def unify = DefaultUnification.unify[Type] _


  /**
   * The default implementation of the unify method,
   * that works with every type.
   * Make sure to always use this to call unify,
   * to make sure the right implementation is used.
   */
  object DefaultUnification extends Unifiable {
    def unify[T](x: T, y: T): (Boolean, Substitution) = (x, y) match {
      case (x1: Unifiable, y1) => x1.unify(x1, y1)
      case (x1, y1: Unifiable) => y1.unify(x1, y1)
      case (x1, y1) => unifyDefault(x1, y1)
    }
  }


  /**
   * A unification returns a boolean and a [[typechecklib.Substitutions.Substitution substitution]].
   * The boolean signals the success.
   * Succeeds if x any y are equal or if every difference
   * has at least a [typechecklib.Types.TyUni TypeVariable]] at either side.
   * Types that need special handling in a Unification
   * can mix-in this trait to override the default behavior.
   */
  trait Unifiable {

    /**
     * Unify arbitrary objects.
     */
    def unify[T](x: T, y: T): (Boolean, Substitution)

    /**
     * Default unification for objects that don't
     * mix-in this trait. Don't call directly, instead
     * use [[typechecklib.Unification.DefaultUnification DefaultUnification.unify]].
     */
    protected def unifyDefault(x: Any, y: Any): (Boolean, Substitution) = {
      (x, y) match {
        case (x, y) if x == y => (true, new Substitution)
        case (x: TyUni, y : Type) => unifyVar(x, y)
        case (x: Type, y: TyUni) => unifyVar(y, x)
        case (x, y) if x.getClass() == y.getClass() &&
          arity(x) == arity(y) => unifyAny(x, y)
        case _ => (false, new Substitution)
      }
    }

    /**
     * Unification for objects that have the same
     * type and number of children.
     * Tries to unify the children.
     */
    protected def unifyAny(x: Any, y: Any): (Boolean, Substitution) = {
      val xc = getChildren(x)
      val yc = getChildren(y)

      if (xc.isEmpty)
        return (x == y, new Substitution)

      val xy = xc zip yc
      val bs = xy map (Function.tupled(unify))
      ((true, new Substitution) /: bs)((b, a) =>
        (b._1 && a._1, b._2 ++ a._2))
    }

    /**
     *  Unification with a TyUni
     */
    protected def unifyVar(x: TyUni, r: Type): (Boolean, Substitution) = {
      if (x == r) {
        (true, new Substitution)
      } else if (occursIn(x, r)) {
        (false, new Substitution)
      } else {
        val sub = new Substitution
        (true, sub + (x -> r))
      }
    }

    /**
     * Whenever x occurs in y
     */
    protected def occursIn[T](x: T, y: T): Boolean = oncebu(
      strategy {
        case a if a == x => Some(a)
      })(y) != None

    /**
     * Number of children.
     */
    protected def arity(x: Any): Int = {
      var c = 0;
      all(queryf {
        case a => c += 1;
      })(x)
      c
    }

    protected def getChildren(x: Any): List[Any] = {
      val l = new ListBuffer[Any]()
      all(queryf {
        case a => l += a
      })(x)
      l.toList
    }
  }
}
