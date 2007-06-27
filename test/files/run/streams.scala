object Test extends Application {
  val s0: Stream[Int] = Stream.empty
  println(s0.take(1))
  println(s0.takeWhile(_ > 0))
  println

  val s1 = Stream.cons(1, Stream.empty)
  println(s1.toArray)
  println(s1.take(1))
  println(s1.take(2))
  println(s1.drop(1))
  println(s1.drop(2))
  println(s1.drop(-1))
  println(s1.dropWhile(_ > 0))
  println

  val s2 = s1.append(Stream.cons(2, Stream.empty))
  println(s2.toArray)
  println(s2.drop(1))
  println(s2.drop(2))
  println(s2.drop(-1))
  println(s2.dropWhile(_ > 0))
  println
}
