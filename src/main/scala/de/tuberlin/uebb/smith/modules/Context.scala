
package de.tuberlin.uebb.smith.modules

import TypeSynonyms._
import scalaz._
import Scalaz._
import scala.collection.immutable.ListMap

/*
 * Context
 */

case class Dictionary(privateMap: IdentifierMapping, publicMap: IdentifierMapping) {
  def apply(id: Identifier, att: Attribute) : Result[Identifier] =
    if(privateMap.contains(id) )
      privateMap(id).successNel[Error]
    else if( publicMap.contains(id))
      publicMap(id).successNel[Error]
    else 
      ContextError("Unknown Identifier: " + id, att).failNel[Identifier]

  def addDef(d: Def, moduleId: Identifier) :Result[Dictionary] = {
    if(privateMap.contains(d.name) || publicMap.contains(d.name))
      ContextError("Duplicated Identifier for Def: " + d.name, d.att).failNel[Dictionary]
    else if (d.isPublic)
      Dictionary(privateMap, publicMap + (d.name -> moduleId~d.name)).successNel[Error]
    else
      Dictionary(privateMap + (d.name -> moduleId~d.name), publicMap).successNel[Error]
  }
}



case class SymbolTable(exprTypes: Map[Identifier,Type],typeKinds:Map[Identifier,Kind])
{
  def +(id: Identifier, t :Type, att: Attribute) : Result[SymbolTable] =
    if(exprTypes.contains(id))
      ContextError("Duplicated Identifier: " + id, att).failNel[SymbolTable]
    else SymbolTable(exprTypes + (id -> t),typeKinds).successNel[Error]

  def +(id: Identifier, k:Kind, att: Attribute) : Result[SymbolTable] =
    if(typeKinds.contains(id))
      ContextError("Duplicated Identifier: " + id, att).failNel[SymbolTable]
    else SymbolTable(exprTypes,typeKinds + (id -> k)).successNel[Error]
}


//we use ListMap to preserve order of modules
case class Context(st:SymbolTable, bindings: Map[Identifier,BindingDef],binder: Map[Identifier,BinderDef], modules: ListMap[Identifier, Module])


object LookUp
{
  def lookUpType(id: Identifier, st: SymbolTable, dic: Dictionary, cm: Identifier, att:Attribute): Result[Type] =
    if (st.exprTypes.contains(id)) st.exprTypes(id).successNel[Error]
    else
      (dic(id, att), dic(cm ~ id, att)) match {
        case (Success(rid), _) => if (st.exprTypes.contains(rid)) (st.exprTypes(rid)).successNel[Error]
        else TypeError("Could not find type for unknown identifier: " + id, att).failNel[Type]
        case (_, Success(rid)) => if (st.exprTypes.contains(rid)) (st.exprTypes(rid)).successNel[Error]
        else TypeError("Could not find type for unknown identifier: " + id, att).failNel[Type]
        case _ => TypeError("Unknown Identifier for Type: " + id, att).failNel[Type]
      }
 
  def lookUpKind(id: Identifier, st: SymbolTable, dic: Dictionary, cm: Identifier, att: Attribute): Result[Kind] =
    if (st.typeKinds.contains(id)) st.typeKinds(id).successNel[Error]
    else
      (dic(id, att), dic(cm ~ id, att)) match {
        case (Success(rid), _) => if (st.typeKinds.contains(rid)) (st.typeKinds(rid)).successNel[Error]
        else TypeError("Could not find kind for unknown identifier: " + id, att).failNel[Kind]
        case (_, Success(rid)) => if (st.typeKinds.contains(rid)) (st.typeKinds(rid)).successNel[Error]
        else TypeError("Could not find kind for unknown identifier: " + id, att).failNel[Kind]
        case _ => TypeError("Could not find kind for unknown identifier: " + id, att).failNel[Kind]
      }

  def lookUpBinder(id: Identifier, cm: Identifier, ctx: Context, dic: Dictionary, att: Attribute): Result[BinderDef] = {
    (dic(id, att), dic(cm ~ id, att)) match {
      case (Success(rid), _) => {
        if (ctx.binder.contains(rid))
          ctx.binder(rid).successNel[Error]
        else TypeError("Unknown Identifier for Binder: " + id, att).failNel[BinderDef]
      }
      case (_, Success(rid)) => {
        if (ctx.binder.contains(rid))
          ctx.binder(rid).successNel[Error]
        else TypeError("Unknown Identifier for Binder: " + id, att).failNel[BinderDef]
      }
      case _ => TypeError("Unknown Identifier for Binder: " + id, att).failNel[BinderDef]
    }
  }
  def lookUpBinding(id: Identifier, cm: Identifier, ctx: Context, dic: Dictionary, att: Attribute): Result[BindingDef] = {
    (dic(id, att), dic(cm ~ id, att)) match {
      case (Success(rid), _) => {
        if (ctx.bindings.contains(rid))
          ctx.bindings(rid).successNel[Error]
        else TypeError("Unknown Identifier for BindingDef: " + id, att).failNel[BindingDef]
      }
      case (_, Success(rid)) => {
        if (ctx.bindings.contains(rid))
          ctx.bindings(rid).successNel[Error]
        else TypeError("Unknown Identifier for BinderingDef: " + id, att).failNel[BindingDef]
      }
      case _ => TypeError("Unknown Identifier for BindingDef: " + id, att).failNel[BindingDef]
    }
  }
}
