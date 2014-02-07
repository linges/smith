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

import scala.language.implicitConversions
import scala.collection.GenTraversableOnce

import org.kiama.rewriting.Rewriter._
import scalaz._
import Scalaz._
import TypeSynonyms._
import Syntax._



  /**
    * A class for immutable substitutions over types.
    */
  case class Substitution(val sub: Map[TyUni, Type]) {
    
    /**
      * Constructor with MapLike Syntax, i.e.
      * {{{
      * val s = new Substitution(x -> y, z -> a)
      * }}}
      */
    def this(elems: (TyUni, Type)*) = this(elems.toMap)


    /**
      * Apply this substitution to the given type variable.
      */
    def apply(tvar: TyUni): Type = sub.getOrElse(tvar, tvar)

    
    /**
      * Generic application of this substitution.
      * 
      * Apply this substitution to an element and all its children. 
      */
    def apply[T](elem: T): T = {
      def applySubst : Strategy = 
        rule { 
          case ty: TyUni => if(sub.contains(ty)) sub(ty) else ty 
          case x => all(applySubst)(x).get
        }
      applySubst(elem).getOrElse(elem) match {
	case t: T @unchecked => t
      }
    }

    
    /**
      * Composition of substitutions, defined as
      * {{{
      * σ ++ φ = v |-> σ(t), for φ(v) = t
      *          v |-> t   , for σ(v) = t and v is not element in the domain of φ
      * }}}
      */
    def ++(that: Substitution): Substitution = {
      // Note: Map.++ is right-biased
      val composedSubstitutions = (this.sub -- that.keys) ++ that.sub.mapValues(this(_))
      
      Substitution(composedSubstitutions)
    }


    /**
      * Add a key and the corresponding value to this Substitution. 
      * If the key already exists, its replaced.
      */
    def +(mapping: (TyUni, Type)) = Substitution(this.sub + mapping)


    /**
      * Remove the mapping containing the given type variable
      * from this substitution.
      */
    def -(tvar: TyUni) = Substitution(this.sub - tvar)


    /**
      * Remove all mappings containing the given type variables
      * from this substitution.
      */
    def --(tvars: GenTraversableOnce[TyUni]) = Substitution(this.sub -- tvars)


    /**
      * Collects all keys of this substitution in an iterable collection.
      */
    def keys: Iterable[TyUni] = sub.keys


    /**
      * Collects all keys of this substitution in a set.
      */
    def keySet: Set[TyUni] = sub.keySet


    override def toString = sub.toString
  }

