package fpinscala.ch02

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  // A definition of factorial, using a local, tail recursive function
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }

  // Another implementation of `factorial`, this time with a `while` loop
  def factorial2(n: Int): Int = {
    var acc = 1
    var i = n
    while (i > 0) {
      acc *= i
      i -= 1
    }
    acc
  }

  // Exercise 1: Write a function to compute the nth fibonacci number
  // 0 1 1 2 3 5 ...
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else go(n - 1, cur, prev + cur)
      // go(4,0,1) = go(3,1,1) = go(2,1,2) = go(1,2,3) = go(0,3,5)
      //  go(1,0,1) = go(0,1,1)
    }
    go(n, 0, 1)
  }

  private def formatAbs(x: Int) = {
    s"The absolute value of ${x} is ${abs(x)}"
  }

  private def formatFactorial(n: Int) = {
    s"The factorial with tailrec of ${n} is ${factorial(n)}"
  }

  private def formatFactorial2(n: Int) = {
    s"The factorial without tailrec of ${n} is ${factorial2(n)}"
  }

  private def formatFibonacci(n: Int) = {
    s"The fibonacci of ${n} is ${fibonacci(n)}"
  }

  def formatResult(descripion: String, n: Int, f: Int => Int) = {
    s"${descripion} $n is ${f(n)} "
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs((-42)))
    println(formatFactorial(5))
    println(formatFactorial2(5))
    println(formatFibonacci(5))
  }
}

object FormatResult {
  import MyModule._

  def main(args: Array[String]): Unit = {
    println("\n==== high order function: ====")

    println(formatResult("The absolute value of", -47, abs))
    println(formatResult("The factorial with tailrec value of", 7, factorial))
    println(
      formatResult("The factorial without tailrec value of", 7, factorial2)
    )
    println(formatResult("The fibonacci value of", 7, fibonacci))
  }
}

// Functions get passed around so often in FP that it's
// convenient to have syntax for constructing a function
// *without* having to give it a name
object AnonymousFunctions {
  import MyModule._

  def main(args: Array[String]): Unit = {
    println(formatResult("Absoulte value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("increment", 10, (x: Int) => { val r = x + 1; r }))
    println(formatResult("increment", 10, (x: Int) => x + 1))
    println(formatResult("increment", 10, x => x + 1))
    println(formatResult("increment", 10, _ + 1))
  }

}

// First, a findFirst, specialized to `String`.
// Ideally, we could generalize this to work for any `Array` type.
object MonomorphicBinarySearch {
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(findFirst(Array("a", "d", "b", "c"), "c"))
  }
}

object PolymorphicFunctions {
  def findFirst[A](as: Array[A], key: A): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (as(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }

  // high order function
  def findFirst2[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted
  def isSort[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else go(n + 1)
    }
    go(0)
  }

  // Polymorphic functions are often so constrained by their type
  // that they only have one implementation! Here's an example:
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise 3: Implement `curry`.
  //
  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // NB: The `Function2` trait has a `curried` method already
  //
  // Exercise 4: Implement `uncurry`
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def main(args: Array[String]): Unit = {
    println(findFirst(Array("a", "d", "b", "c"), "c"))
    println(findFirst(Array(1, 4, 5, 8, 9), 9))

    println("\n==== high order function: ====")
    println(findFirst2(Array(1, 7, 5, 8, 9), (x: Int) => x % 2 == 0))
    println(findFirst2(Array("a", "d", "b", "c"), (x: String) => x == "d"))

    println(isSort(Array(1, 7, 5, 8, 9), (x: Int, y: Int) => x < y))
    println(isSort(Array(1, 7, 5, 8, 9), (x: Int, y: Int) => x > y))
    println(isSort(Array(1, 3, 5, 7, 9), (x: Int, y: Int) => x > y))
    println(isSort(Array(1, 3, 5, 7, 9), (x: Int, y: Int) => x < y))
  }
}
