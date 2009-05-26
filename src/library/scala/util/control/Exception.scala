package scala.util.control

/** Classes representing the components of exception handling.
 *  Each class is independently composable.  Some common uses:
 *  
 *  <pre>
 *  <b>import</b> scala.util.control.Exception._
 *  <b>import</b> java.net._
 *
 *  <b>val</b> x1 = catching(classOf[MalformedURLException]) opt new URL(s)
 *  <b>val</b> x2 = catching(classOf[MalformedURLException], classOf[NullPointerException]) either new URL(s)
 *  </pre>
 *  @author Paul Phillips
 */

import java.lang.reflect.InvocationTargetException

object Exception
{
  trait Described { 
    protected val name: String
    private var _desc: String = ""
    def desc = _desc
    def withDesc(s: String): this.type = {
      _desc = s
      this
    }
    override def toString() = name + "(" + desc + ")"
  }
  
  /** A container class for finally code. */
  class Finally(fin: => Unit) extends Described {
    protected val name = "Finally"
    
    def butFirst(fin2: => Unit) = new Finally({ fin2 ; fin })
    def  andAlso(fin2: => Unit) = new Finally({ fin ; fin2 })
    def invoke() { fin }
  }
  
  /** A container class for catch logic. */
  class Catch[+T](val pf: PartialFunction[Throwable, T]) extends Described {  
    protected val name = "Catch"
    
    /** Create a new Catch with additional exception handling logic. */
    def orElse[U >: T](pf2: PartialFunction[Throwable, U]): Catch[U] = new Catch(pf orElse pf2)
    def orElse[U >: T](catch2: Catch[U]): Catch[U] = orElse(catch2.pf)
    def orElse[U >: T](res: U, exes: Class[_ <: Throwable]*): Catch[U] =
      orElse(pfFromExceptionsWithResult(_ => res, exes : _*))
    
    /** Invoke this catch logic upon the supplied body. */
    def invokeOn[U >: T](body: => U): U =
      try     { body }
      catch   { case e if pf.isDefinedAt(e) => pf(e) }
    
    /** Invoke this catch logic upon the supplied body, mapping the result
     *  into Option[T] - None if any exception was caught, Some(T) otherwise.
     */
    def opt[U >: T](body: => U): Option[U] = toOption invokeOn Some(body)
    
    /** Invoke this catch logic upon the supplied body, mapping the result
     *  into Either[Throwable, T] - Left(exception) if an exception was caught,
     *  Right(T) otherwise.
     */
    def either[U >: T](body: => U): Either[Throwable, U] = toEither invokeOn Right(body)
    
    /** Create a new Catch with the same isDefinedAt logic as this one,
      * but with the supplied apply method replacing the current one. */
    def withApply[U](f: (Throwable) => U): Catch[U] = {
      val pf2 = new PartialFunction[Throwable, U] {
        def isDefinedAt(x: Throwable) = pf isDefinedAt x
        def apply(x: Throwable) = f(x)
      }
      new Catch(pf2)
    }

    /** Convenience methods. */
    def toOption: Catch[Option[T]] = withApply(_ => None)
    def toEither: Catch[Either[Throwable, T]] = withApply(Left(_))
  }

  /** A container class for try/catch/finally logic. */
  class Try[+T](body: => T, val catcher: Catch[T], val fin: Finally) {
    /** Invoke "body" using catch logic "catcher" and finally "fin" */
    def invoke(): T                     = withFin { catcher invokeOn body }
    
    /** As invoke, but map caught exceptions to None and success to Some(T) */
    def opt(): Option[T]                = withFin { catcher opt body }
    
    /** As invoke, but map caught exceptions to Left(ex) and success to Right(x) */
    def either(): Either[Throwable, T]  = withFin { catcher either body }
    
    /** Create a new Try with the supplied body replacing the current body */
    def tryInstead[U >: T](body2: => U) = new Try(body2, catcher, fin)
    
    /** Create a new Try with the supplied Catch replacing the current Catch */
    def catchInstead[U >: T](catcher2: Catch[U]) = new Try(body, catcher2, fin)
    
    /** Create a new Try with the supplied Finally replacing the current Finally */
    def finInstead(fin2: Finally) = new Try(body, catcher, fin2)
    
    /** Create a new Try with the supplied logic appended to the existing Catch logic. */
    def catchAlso[U >: T](pf2: PartialFunction[Throwable, U]) =
      new Try(body, catcher orElse pf2, fin)

    /** Create a new Try with the supplied code appended to the existing Finally. */
    def finAlso(fin2: => Unit) = new Try(body, catcher, fin andAlso fin2)
    
    override def toString() = List("Try(<body>)", catcher.toString, fin.toString) mkString " "
    private def withFin[T](f: => T) =
      try     { f }
      finally { fin.invoke() }
  }

  /** The empty Catch object. */
  final val noCatch = new Catch(
    new PartialFunction[Throwable, Nothing] {
      def isDefinedAt(x: Throwable) = false
      def apply(x: Throwable) = throw x
    }
  ) withDesc "<nothing>"
  
  /** The empty Finally object. */
  final val noFinally = new Finally(()) withDesc "()"
  
  /** Creates a Catch object which will catch any of the supplied exceptions.
    * Since the returned Catch object has no specific logic defined and will simply
    * rethrow the exceptions it catches, you will typically want to call "opt" or
    * "either" on the return value, or assign custom logic by calling "withApply".
    */
  def catching[T](exceptions: Class[_ <: Throwable]*): Catch[T] =
    new Catch(pfFromExceptions(exceptions : _*)) withDesc exceptions.map(_.getName).mkString(", ")

  /** Create a Try object with the supplied body and Catch logic. */
  def tryCatch[T](body: => T)(pf: PartialFunction[Throwable, T]) =
    new Try(body, new Catch(pf), noFinally)

  /** Create a Try object with the supplied body and Finally code. */
  def tryFin[T](body: => T)(fin: => Unit) =
    new Try(body, noCatch, new Finally(fin))
    
  /** Create a Try object with the supplied body, Catch logic, and Finally code. */
  def tryCatchFin[T](body: => T)(pf: PartialFunction[Throwable, T])(fin: => Unit) =
    new Try(body, new Catch(pf), new Finally(fin))
    
  val reflectionUnwrapper: Catch[Nothing] = {
    // unwrap unhelpful nested exceptions so the most interesting one is reported
    def unwrap(e: Throwable): Throwable = e match {
      case (_: InvocationTargetException | _: ExceptionInInitializerError) if e.getCause ne null =>
        unwrap(e.getCause)
      case _ => e
    }
    
    new Catch({ case e => throw unwrap(e) })
  }

  /** Private **/
  
  private def pfFromExceptionsWithResult[T](f: (Throwable) => T, exceptions: Class[_ <: Throwable]*) =
    new PartialFunction[Throwable, T] {
      def apply(ex: Throwable) = f(ex)
      def isDefinedAt(x: Throwable) = exceptions exists (_ isAssignableFrom x.getClass)
    }
  
  private def pfFromExceptions(exceptions: Class[_ <: Throwable]*) = 
    pfFromExceptionsWithResult[Nothing](throw _, exceptions : _*)
}
