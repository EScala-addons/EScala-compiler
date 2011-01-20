/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// generated by genprod on Sat Oct 16 11:19:09 PDT 2010  

package scala



object Product4 {
  def unapply[T1, T2, T3, T4](x: Product4[T1, T2, T3, T4]): Option[Product4[T1, T2, T3, T4]] = 
    Some(x)
}

/** Product4 is a cartesian product of 4 components.
 *  
 *  @since 2.3
 */
trait Product4[+T1, +T2, +T3, +T4] extends Product {
  /**
   *  The arity of this product.
   *  @return 4
   */
  override def productArity = 4

  
  /**
   *  Returns the n-th projection of this product if 0 < n <= productArity,
   *  otherwise throws IndexOutOfBoundsException.
   *
   *  @param n number of the projection to be returned 
   *  @return  same as _(n+1)
   *  @throws  IndexOutOfBoundsException
   */  

  @throws(classOf[IndexOutOfBoundsException])
  override def productElement(n: Int) = n match { 
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case _ => throw new IndexOutOfBoundsException(n.toString())
 }  

  /** projection of this product */
  def _1: T1

  /** projection of this product */
  def _2: T2

  /** projection of this product */
  def _3: T3

  /** projection of this product */
  def _4: T4



}
