// author: buraq 


// This program generates the Product$i classes, where 0 <= i <+ MAXWIDTH

// usage: scala -classpath ... genprod PATH
//   where PATH is the desired output directory

// $Id$
object genprod {

  /** The biggest ?? has Sup?? - 1 components/arguments */
  val SUP_PRODUCT_ARITY   = 23
  val SUP_TUPLE_ARITY     = 9
  val SUP_FUNCTION_ARITY  = 9

  def productClassname(i:Int) = "Product"+i

  def productFilename(i:Int) = productClassname(i)+".scala"

  def tupleClassname(i:Int) = "Tuple"+i

  def tupleFilename(i:Int) = tupleClassname(i)+".scala"

  def functionClassname(i:Int) = "Function"+i

  def functionFilename(i:Int) = functionClassname(i)+".scala"

  def targs(i:Int) = 
    for (val j <- List.range(1,i+1)) yield  "T" + j

  def covariantArgs(i:Int) = 
             for (val t <- targs(i)) yield  "+" + t

  def contraCoArgs(i:Int) = 
            (for (val t <- targs(i)) yield  "-" + t):::List("+R")

  def vdefs(i: Int) = 
    for (val j <- List.range(1,i+1)) yield "v" + j

  def mdefs(i: Int) = 
    for (val j <- List.range(1,i+1)) yield "_" + j


  def zippedAndCommaSeparated (left: List[String], right: List[String]): String = {
    val sb = new StringBuffer()
    val it = (left zip right).elements
    def append1 = {
      val p = it.next
      sb.append(p._1).append(':').append(p._2)
    }
    if(it.hasNext) {
      append1
      while(it.hasNext) { sb.append(", "); append1 }
    }
    sb.toString
  }
  def fields (i:Int) = zippedAndCommaSeparated( mdefs(i), targs(i) )

  def funArgs(i:Int) = zippedAndCommaSeparated( vdefs(i),  targs(i) )

  def productFiles = 
    for(val i <- List.range(0, SUP_PRODUCT_ARITY)) yield ProductFile.make(i) ;

  def tupleFiles = 
    for(val i <- List.range(1, SUP_TUPLE_ARITY)) yield TupleFile.make(i);
    
  def functionFiles = 
    for(val i <- List.range(0, SUP_FUNCTION_ARITY)) yield FunctionFile.make(i)
  
  def allfiles = 
    productFiles ::: tupleFiles ::: functionFiles

  def main(args:Array[String]) = {
    if(args.length != 1) {
      Console.println("please give path of output directory")
      System.exit(-1)
    }
    import java.io.{ File, FileOutputStream }
  import java.nio.channels.Channels
    val out = args(0)
    for(val node <- allfiles) {
      val f = new File(out + File.separator + node.attributes("name"))
      f.createNewFile
      val fos = new FileOutputStream(f)
      val c = fos.getChannel
      val w = Channels.newWriter(c, "utf-8")
      w.write(node.text)
      w.close
    }
  }
}

/* zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 
                             F U N C T I O N 
zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz */

object FunctionFile {
  import genprod._
  def make(i: Int) = {
    val __typeArgs__ = contraCoArgs(i).mkString("[",", ","]")
    val __funArgs__ = funArgs(i)
<file name={functionFilename(i)}>
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// generated on {new java.util.Date().toString()} {if(descriptiveComment(i).length > 0) "(with fancy comment)" else ""} {if(moreMethods(i).length > 0) "(with extra methods)" else ""}
// $Id$

package scala


/**
 * Function with {i} parameters. {DescriptiveComment.forFunction(i)}
 */
trait {functionClassname(i)} {__typeArgs__} extends AnyRef {{
  def apply({__funArgs__}): R
  override def toString() = "&lt;function>"
  {moreMethods(i)}
}}
</file>
}

  def moreMethods(i:Int) = i match {
    case 1 => """
  def compose[A](g: A => T1): A => R = { x => apply(g(x)) }
  def andThen[A](g: R => A): T1 => A = g compose this
"""
    case _ => ""
  }
  def descriptiveComment(i:Int) = i match {
    case 0 => """In the following example the definition of
 * <code>currentSeconds</code> is a shorthand for the anonymous class
 * definition <code>anonfun0</code>:
 * <pre>
 * <b>object</b> Main <b>extends</b> Application {
 *
 *   <b>val</b> currentSeconds = () => System.currentTimeMillis() / 1000L
 *
 *   <b>val</b> anonfun0 = <b>new</b> Function0[Long] {
 *     <b>def</b> apply(): Long = System.currentTimeMillis() / 1000L
 *   }
 *
 *   Console.println(currentSeconds())
 *   Console.println(anonfun0())
 * }</pre>"""
    case 1 => """In the following example the definition of
 * <code>succ</code> is a shorthand for the anonymous class definition
 * <code>anonfun1</code>:
 * <pre>
 * <b>object</b> Main <b>extends</b> Application {
 *
 *   <b>val</b> succ = (x: Int) => x + 1
 *
 *   <b>val</b> anonfun1 = <b>new</b> Function1[Int, Int] {
 *     <b>def</b> apply(x: Int): Int = x + 1
 *   }
 *
 *   Console.println(succ(0))
 *   Console.println(anonfun1(0))
 * }</pre>"""
    case 2 => """In the following example the definition of
 * <code>max</code> is a shorthand for the anonymous class definition
 * <code>anonfun2</code>:
 * <pre>
 * <b>object</b> Main <b>extends</b> Application {
 *
 *   <b>val</b> max = (x: Int, y: Int) => <b>if</b> (x < y) y <b>else</b> x
 *
 *   <b>val</b> anonfun2 = <b>new</b> Function2[Int, Int, Int] {
 *     <b>def</b> apply(x: Int, y: Int): Int = <b>if</b> (x < y) y <b>else</b> x
 *   }
 *
 *   Console.println(max(0, 1))
 *   Console.println(anonfun2(0, 1))
 * }</pre>"""
    case _ => ""
  }

} // object FunctionFile
/* zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 
                                     T U P L E
zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz */

object TupleFile {
  import genprod._
  def make(i: Int) = {  
      val __typeArgs__ = covariantArgs(i).mkString("[",", ","]")
      val __fields__ = fields(i)
<file name={tupleFilename(i)}>
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

// generated on {new java.util.Date().toString()}
package scala;

/** {tupleClassname(i)} is the canonical representation of a @see {productClassname(i)} */
case class {tupleClassname(i)} {__typeArgs__} ({ __fields__ }) {{

  override def productPrefix = ""

}}
</file>
    }
} // object TupleFile



/* zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz 
                                  P R O D U C T
zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz */
object ProductFile {
  import genprod._
  def make(i:Int) = {
    val __typeArgs__ = if(i==0) Nil else covariantArgs(i).mkString("[",", ","]")
<file name={productFilename(i)}>
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// generated on {new java.util.Date().toString()}
package scala

import Predef._

/** {productClassname(i)} is a cartesian product of {i} components 
 */
trait {productClassname(i)} {__typeArgs__} extends Product {{

  /** 
   *  The arity of this product.
   *  @return {i}
   */
  override def arity = {i}

  /** 
   *  Returns the n-th projection of this product if 0&lt;n&lt;=arity, otherwise null
   *  @param n number of the projection to be returned
   *  @throws IndexOutOfBoundsException
   */
  override def element(n: Int) = n match {{
    {for(val Tuple2(m,j) <- mdefs(i).zip(List.range(1,i+1)))
     yield "case "+j+" => "+m+"\n    "}case _ => throw new IndexOutOfBoundsException(n.toString())
  }}

  {for(val Tuple2(m,t) <- mdefs(i) zip targs(i)) yield 
    "/** projection of this product */\n  def "+m+":"+t+"\n\n  " }
}}
</file>
  }
}
