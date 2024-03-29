/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection

import generic._
import mutable.{ Builder, MapBuilder }
import annotation.migration

/** A template trait for maps, which associate keys with values.
 *
 *  $mapNote 
 *  $mapTags
 *  @since 2.8
 *
 *  @define mapNote
 *    '''Implementation note:'''
 *    This trait provides most of the operations of a `Map` independently of its representation.
 *    It is typically inherited by concrete implementations of maps.
 *
 *    To implement a concrete map, you need to provide implementations of the
 *    following methods:
 *    {{{
 *       def get(key: A): Option[B]
 *       def iterator: Iterator[(A, B)]
 *       def + [B1 >: B](kv: (A, B1)): This
 *       def -(key: A): This
 *    }}}
 *    If you wish that methods like `take`, `drop`, `filter` also return the same kind of map
 *    you should also override:
 *    {{{
 *       def empty: This
 *    }}}
 *    It is also good idea to override methods `foreach` and
 *    `size` for efficiency.
 * 
 *  @define mapTags
 *  @tparam A    the type of the keys.
 *  @tparam B    the type of associated values.
 *  @tparam This the type of the map itself.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 * 
 *  @define coll map
 *  @define Coll Map
 *  @define willNotTerminateInf
 *  @define mayNotTerminateInf
 */
trait MapLike[A, +B, +This <: MapLike[A, B, This] with Map[A, B]]
  extends PartialFunction[A, B] 
     with IterableLike[(A, B), This] 
     with Subtractable[A, This] { 
self =>

  /** The empty map of the same type as this map
   *   @return   an empty map of type `This`.
   */
  def empty: This
       
  /** A common implementation of `newBuilder` for all maps in terms of `empty`.
   *  Overridden for mutable maps in `mutable.MapLike`.
   */
  override protected[this] def newBuilder: Builder[(A, B), This] = new MapBuilder[A, B, This](empty)

  /** Optionally returns the value associated with a key.
   *
   *  @param  key    the key value
   *  @return an option value containing the value associated with `key` in this map,
   *          or `None` if none exists.
   */
  def get(key: A): Option[B]

  /** Creates a new iterator over all key/value pairs of this map
   *  
   *  @return the new iterator
   */
  def iterator: Iterator[(A, B)]
     
  /** Adds a key/value pair to this map, returning a new map. 
   *  @param    kv the key/value pair
   *  @tparam   B1 the type of the value in the key/value pair. 
   *  @return   a new map with the new binding added to this map
   *  @usecase  def + (kv: (A, B)): Map[A, B]
   */
  def + [B1 >: B] (kv: (A, B1)): Map[A, B1]

  /** Removes a key from this map, returning a new map.
   *  @param    key the key to be removed
   *  @return   a new map without a binding for `key`
   *  @usecase  def - (key: A): Map[A, B]
   */
  def - (key: A): This

  /** Tests whether the map is empty.
   *
   *  @return `true` if the map does not contain any key/value binding, `false` otherwise.
   */
  override def isEmpty: Boolean = size == 0

  /**  Returns the value associated with a key, or a default value if the key is not contained in the map. 
   *   @param   key      the key.
   *   @param   default  a computation that yields a default value in case no binding for `key` is
   *                     found in the map.
   *   @tparam  B1       the result type of the default computation. 
   *   @return  the value associated with `key` if it exists,
   *            otherwise the result of the `default` computation.
   *   @usecase def getOrElse(key: A, default: => B): B
   */
  def getOrElse[B1 >: B](key: A, default: => B1): B1 = get(key) match {
    case Some(v) => v
    case None => default 
  }

  /** Retrieves the value which is associated with the given key. This
   *  method invokes the `default` method of the map if there is no mapping
   *  from the given key to a value. Unless overridden, the `default` method throws a
   *  `NoSuchElementException`.
   *
   *  @param  key the key
   *  @return     the value associated with the given key, or the result of the
   *              map's `default` method, if none exists.
   */
  def apply(key: A): B = get(key) match {
    case None => default(key)
    case Some(value) => value
  }

  /** Tests whether this map contains a binding for a key.
   *
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this map, `false` otherwise.
   */
  def contains(key: A): Boolean = get(key) match {
    case None => false
    case Some(_) => true
  }

  /** Tests whether this map contains a binding for a key. This method,
   *  which implements an abstract method of trait `PartialFunction`,
   *  is equivalent to `contains`.
   *
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this map, `false` otherwise.
   */
  def isDefinedAt(key: A) = contains(key)

  /** Collects all keys of this map in a set.
   * @return  a set containing all keys of this map.
   */
  def keySet: Set[A] = new DefaultKeySet

  /** The implementation class of the set returned by `keySet`.
   */
  protected class DefaultKeySet extends Set[A] {
    def contains(key : A) = self.contains(key)
    def iterator = keysIterator
    def + (elem: A): Set[A] = (Set[A]() ++ this + elem).asInstanceOf[Set[A]] // !!! concrete overrides abstract problem
    def - (elem: A): Set[A] = (Set[A]() ++ this - elem).asInstanceOf[Set[A]] // !!! concrete overrides abstract problem
    override def size = self.size
    override def foreach[C](f: A => C) = for ((k, v) <- self) f(k)
  }

  /** Creates an iterator for all keys.
   *
   *  @return an iterator over all keys.
   */
  def keysIterator: Iterator[A] = new Iterator[A] {
    val iter = self.iterator
    def hasNext = iter.hasNext
    def next = iter.next._1
  }

  /** Creates an iterator for all keys.
   *
   *  @return an iterator over all keys.
   */
  @migration(2, 8, "As of 2.8, keys returns Iterable[A] rather than Iterator[A].")
  def keys: Iterable[A] = keySet

  /** Collects all values of this map in an iterable collection. 
   * @return the values of this map as an iterable.
   */
  @migration(2, 8, "As of 2.8, values returns Iterable[B] rather than Iterator[B].")
  def values: Iterable[B] = new DefaultValuesIterable

  /** The implementation class of the iterable returned by `values`.
   */
  protected class DefaultValuesIterable extends Iterable[B] {
    def iterator = valuesIterator
    override def size = self.size
    override def foreach[C](f: B => C) = for ((k, v) <- self) f(v)
  }

  /** Creates an iterator for all values in this map.
   *
   *  @return an iterator over all values that are associated with some key in this map.
   */
  def valuesIterator: Iterator[B] = new Iterator[B] {
    val iter = self.iterator
    def hasNext = iter.hasNext
    def next = iter.next._2
  }

  /** Defines the default value computation for the map,
   *  returned when a key is not found
   *  The method implemented here throws an exception,
   *  but it might be overridden in subclasses.
   *
   *  @param key the given key value for which a binding is missing.
   *  @throws `NoSuchElementException`
   */
  def default(key: A): B =
    throw new NoSuchElementException("key not found: " + key)
    
  /** Filters this map by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
   *          the predicate `p`. The resulting map wraps the original map without copying any elements.
   */
  def filterKeys(p: A => Boolean): Map[A, B] = new DefaultMap[A, B] {
    override def foreach[C](f: ((A, B)) => C): Unit = for (kv <- self) if (p(kv._1)) f(kv)
    def iterator = self.iterator.filter(kv => p(kv._1))
    override def contains(key: A) = self.contains(key) && p(key)
    def get(key: A) = if (!p(key)) None else self.get(key)
  }    

  /** Transforms this map by applying a function to every retrieved value.
   *  @param  f   the function used to transform values of this map.
   *  @return a map view which maps every key of this map
   *          to `f(this(key))`. The resulting map wraps the original map without copying any elements.
   */
  def mapValues[C](f: B => C): Map[A, C] = new DefaultMap[A, C] {
    override def foreach[D](g: ((A, C)) => D): Unit = for ((k, v) <- self) g((k, f(v)))
    def iterator = for ((k, v) <- self.iterator) yield (k, f(v))
    override def size = self.size
    override def contains(key: A) = self.contains(key)
    def get(key: A) = self.get(key).map(f)
  }

  @deprecated("use `mapValues' instead") def mapElements[C](f: B => C) = mapValues(f)

  // The following 5 operations (updated, two times +, two times ++) should really be
  // generic, returning This[B]. We need better covariance support to express that though.
  // So right now we do the brute force approach of code duplication.

  /** Creates a new map obtained by updating this map with a given key/value pair. 
   *  @param    key the key
   *  @param    value the value
   *  @tparam   B1 the type of the added value
   *  @return   A new map with the new key/value mapping added to this map.
   *  @usecase  def updated(key: A, value: B): Map[A, B]
   */
  def updated [B1 >: B](key: A, value: B1): Map[A, B1] = this + ((key, value))

  /** Adds key/value pairs to this map, returning a new map.
   *
   *  This method takes two or more key/value pairs. Another overloaded
   *  variant of this method handles the case where a single key/value pair is
   *  added.
   *  @param    kv1 the first key/value pair
   *  @param    kv2 the second key/value pair
   *  @param    kvs the remaining key/value pairs
   *  @tparam   B1  the type of the added values
   *  @return   a new map with the given bindings added to this map
   *  @usecase  def + (kvs: (A, B)*): Map[A, B]
   *  @param    the key/value pairs
   */
  def + [B1 >: B] (kv1: (A, B1), kv2: (A, B1), kvs: (A, B1) *): Map[A, B1] =
    this + kv1 + kv2 ++ kvs

  /** Adds all key/value pairs in a traversable collection to this map, returning a new map.
   *
   *  @param    kvs the collection containing the added key/value pairs
   *  @tparam   B1  the type of the added values
   *  @return   a new map with the given bindings added to this map
   *  @usecase  def ++ (xs: Traversable[(A, B)]): Map[A, B]
   */
  def ++[B1 >: B](xs: TraversableOnce[(A, B1)]): Map[A, B1] = 
    ((repr: Map[A, B1]) /: xs) (_ + _)

  /** Returns a new map with all key/value pairs for which the predicate
   *  `p` returns `true`.
   *
   *  '''Note:'''    This method works by successively removing elements fro which the
   *           predicate is false from this set.
   *           If removal is slow, or you expect that most elements of the set
   *           will be removed, you might consider using `filter`
   *           with a negated predicate instead. 
   *  @param p    A predicate over key-value pairs
   *  @return     A new map containing elements not satisfying the predicate.
   */
  override def filterNot(p: ((A, B)) => Boolean): This = {
    var res: This = repr
    for (kv <- this)
      if (p(kv)) res = (res - kv._1).asInstanceOf[This] // !!! concrete overrides abstract problem
    res
  }
  
  /** Overridden for efficiency. */
  override def toSeq: Seq[(A, B)] = toBuffer[(A, B)]
  override def toBuffer[C >: (A, B)]: mutable.Buffer[C] = {
    val result = new mutable.ArrayBuffer[C](size)
    copyToBuffer(result)
    result
  }

  /** Appends all bindings of this map to a string builder using start, end, and separator strings.
   *  The written text begins with the string `start` and ends with the string
   *  `end`. Inside, the string representations of all bindings of this map
   *  in the form of `key -> value` are separated by the string `sep`.
   *   
   *  @param b     the builder to which strings are appended.
   *  @param start the starting string.
   *  @param sep   the separator string.
   *  @param end   the ending string.
   *  @return      the string builder `b` to which elements were appended.
   */
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    this.iterator.map { case (k, v) => k+" -> "+v }.addString(b, start, sep, end)

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this $coll.
   *           Unless overridden in subclasses, the string prefix of every map is `"Map"`.
   */
  override def stringPrefix: String = "Map"

  override /*PartialFunction*/
  def toString = super[IterableLike].toString
  
  // This hash code must be symmetric in the contents but ought not
  // collide trivially.
  override def hashCode() = util.MurmurHash.symmetricHash(this,Map.hashSeed)
  
  /** Compares two maps structurally; i.e. checks if all mappings
   *  contained in this map are also contained in the other map,
   *  and vice versa.
   *
   *  @param that the other map
   *  @return     `true` if both maps contain exactly the
   *              same mappings, `false` otherwise.
   */
  override def equals(that: Any): Boolean = that match {
    case that: Map[b, _] => 
      (this eq that) ||
      (that canEqual this) &&
      (this.size == that.size) && {
      try {
        this forall { 
          case (k, v) => that.get(k.asInstanceOf[b]) match {
            case Some(`v`) =>
              true
            case _ => false
          }
        }
      } catch { 
        case ex: ClassCastException => 
          println("class cast "); false 
      }}
    case _ =>
      false
  }
}
