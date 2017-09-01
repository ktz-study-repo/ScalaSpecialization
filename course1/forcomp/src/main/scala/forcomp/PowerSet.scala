package forcomp

object PowerSet extends App{
  def power[A](t: Set[A]): Set[Set[A]] = {
    @annotation.tailrec
    def pwr(t: Set[A], ps: Set[Set[A]]): Set[Set[A]] =
      if (t.isEmpty)
        ps
      else
        pwr(t.tail, ps ++ (ps map (_ + t.head)))
    pwr(t, Set(Set.empty[A])) //Powerset of ∅ is {∅}
  }

  power(Set(1,2,3))


  def listLoop(n: Int): List[Int] =
    if(n == 0) Nil
    else n :: listLoop(n - 1)

  println(listLoop(10))
}
