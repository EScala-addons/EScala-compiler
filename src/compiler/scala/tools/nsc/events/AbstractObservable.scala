package scala.tools.nsc
package events

import transform._
import symtab._
import Flags._
import util.EventUtil
import typechecker._

/**
 * This trait contains the method responsible for the instrumentation of abstract methods.
 * It also provides methods to check whether the a subclass must implement the instrumentation.
 *
 * @author Lucas Satabin
 */
trait AbstractObservable {
  this: ObservableInstrumentation =>

  import global._
  import definitions._

  /**
   * This methods generates the synthesized elements for an instrumented abstract method:
   * <pre>
   * <instrumented> <mods> def m(v1: T1, ..., vk: Tk): T
   * </pre>
   * is transformed to
   * <pre>
   * <mods \ observable> imperative evt m$T1$...$Tk$before[(T1, ..., Tk)]
   * <mods \ observable> imperative evt m$T1$...$Tk$after[((T1, ..., Tk), T)]
   * <mods \ observable> imperative evt m$T1$...$Tk$error[(T1, ..., Tk)]
   * <instrumented> <mods> def m(v1: T1, ..., vk: Tk): T
   * </pre>
   * The instrumentation itself is realized in the concrete implementation of the method.
   */
  def instrumentAbstract(meth: Symbol): (DefDef, ValDef, ValDef, ValDef) = {
    null
  }

  /**
   * This method generates the elements for a concrete instrumented method being the 
   * implementation of an abstract instrumented method
   * <pre>
   * <instrumented> <mods> def m(v1: T1, ..., vk: Tk): T = {
   *   body
   * }
   * </pre>
   * is transformed to
   * <pre>
   * <instrumented> <mods> def m(v1: T1, ..., vk: Tk): T = {
   *   m$T1$...$Tk$before(v1, ..., vk)
   *   try {
   *     val res = m$T1$...$Tk$impl(v1, ..., vk)
   *     m$T1$...$Tk$after(v1, ..., vk, res)
   *   } catch {
   *     case t =>
   *       m$T1$...$Tk$error(v1, ..., vk)
   *       throw t
   *   }
   * }
   * override protected <mods \ observable> def m$T1$...$Tk$impl(v1: T1, ..., vk: Tk): T = body
   * </pre>
   */
  def instrumentOverridingAbstractObs(meth: DefDef): (DefDef, DefDef) = {
    null
  }

  /**
   * Indicates whether at least one super abstract declaration is instrumented
   * for example in this case
   * trait T1 { def m() }
   * trait T2 extends T1 { observable def m() }
   * trait T3 extends T1 { def m { ... } }
   * class C extends T1 with T2 with T3
   * In this case, method `m' in C must be instrumented because one super class (here T2)
   * declares the method as observable and abstract
   * @param meth the method to check
   * @param in from this class
   */
  def hasSuperAbstractInstr(meth: Symbol, in: Symbol): Boolean = {

    false
  }

}

// vim: set ts=4 sw=4 et:
