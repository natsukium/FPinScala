object Exercise {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev1: Int, prev2: Int): Int = {
      if (n == 1)
        0
      else if (n == 2)
        prev1
      else
        loop(n - 1, prev1 + prev2, prev1)
    }
    loop(n, 1, 0)
  }

  def testFib() = {
    assert(fib(1) == 0)
    assert(fib(2) == 1)
    assert(fib(3) == 1)
    assert(fib(4) == 2)
    assert(fib(5) == 3)
    assert(fib(10) == 34)
  }

  def isSorted[A](arr: Array[A], f: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n + 1 >= arr.length)
        true
      else if (f(arr(n), arr(n + 1)))
        loop(n + 1)
      else
        false
    }
    loop(0)
  }

  def testIsSorted() = {
    assert(!isSorted(Array(1, 2, 4, 3), (x: Int, y: Int) => x < y))
    assert(isSorted(Array('a', 'b', 'c'), (x: Char, y: Char) => x < y))
    assert(
      !isSorted(
        Array("dog", "cat", "mouse"),
        (x: String, y: String) => x < y
      )
    )
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def testCurry() = {
    def add(x: Int, y: Int) = x + y
    var curriedAdd = curry(add)
    assert(curriedAdd(1)(2) == add(1, 2))
    assert(uncurry(curriedAdd)(1, 2) == add(1, 2))
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def testCompose() = {
    def f(x: Int) = x * 2
    def g(x: Int) = x + 2
    assert(compose(f, g)(2) == f(g(2)))
  }

  def main(args: Array[String]): Unit = {
    testFib()
    testIsSorted()
    testCurry()
    testCompose()
  }
}
