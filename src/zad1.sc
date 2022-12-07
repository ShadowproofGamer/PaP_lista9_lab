def lfrom(n:Int):Stream[Double] = {
  n #:: lfrom(n + 1)
}

// Zad 1
def lrepeatExponential(n: Int, s: Stream[Double]): Stream[Double] = {
  def lrepeatExponential_rec(i: Int, x: Double, init: Double, remaining: () => Stream[Double]): Stream[Double] = {
    if (i < n) {
      (x*init) #:: lrepeatExponential_rec(i + 1, x*init, init, remaining)
    } else {
      (x*init) #:: remaining()
    }
  }

  if (s.isEmpty) {
    Stream.empty
  } else {
    val x #:: xs = s
    lrepeatExponential_rec(1, 1, x, () => lrepeatExponential(n+1, xs))
  }
}

println(lrepeatExponential(1, lfrom(1)).take(15).toList)