/*
FOR HW2
use fold?
split :"this is a text" => list of lists of chars/strings ["s","a","text"]
current element: "i" (is not a separator) => append "i" to the list above
you first recursively solve the small problem and the append "i"

then:
["is","a","text"]
current elem: white space (separator) => we put Nil => [Nil","is","a","text"]

its the same ide as computing the factorial
use patterns to deal with situations (5 patterns?)
 */

/*
POLYMORPHISM "many forms"

A) Subtype polymorphism
A type S is related to another type T (supertype) by some notion
of substitutability, meaning that function written to operate on elements
of type T (supertype) can also operate on elements of type S (subtype).
 */

class Animal{
  def scream: String = "An animal screams!"
}

class Wolf extends Animal {
  override def scream: String = "A wolf screams"
}

new Wolf().scream
new Animal().scream

/*
B) Ad-hoc polymorphism (overloading)
  Is obtained when a function works or appears to work on several different types
(which may not have anything in common), and that function may behave in unrelated ways,
for each type.
 e.g.:"+" works for ints, double, strings, etc but it is implemented in a different way
 */

/*
C) Parametric polymorphism - Scala generics
Functions (or data types) which can be define generically so that they work
"identically" on families of types without depending on them.

Examples:
  -computing the size of a list
  -many other operations working on lists, and NOT DEPENDING on the contained type of the list
 */

/* diff between ad hoc and parametric polymorphism
  -AD-HOC => "single name" -> "multiple implementations"
  -Parametric => "single name" -> "single implementation"
 */

/*
"Scala generics" (a little different from Java Generics)
We will implement a very simple generic type:
 -
 */

trait FList[A] { // a polymorphic trait, it depends on some type A
  def length:Int
  def head: A
  def tail: FList[A]
  def map[B](f:A=>B):FList[B]
  //FOR MAP:
  // super useful on lists because we can have a function f and [a,b,c,d] =>[f(a),f(b),f(c),f(d)] f:A->B
  // B is another type variable and is not part of the list, its part of the map function
  def foldRight[B](acc:A)(op: (A,B)=>B): B
  //foldRight -useful for traversing
  // acc, op [a,b,c]
  //a op(b op(c op acc ))

  def foldLeft[B](acc:B)(op: (B,A)=>B): B
  //foldLeft
  //acc op(a op(b op c))
  def contains(e:A): Boolean =
    this.foldRight(false)(_ == e || _)//if(acc) true else (x==e))

  def indexOf(e:A):Int
  def update(e:A, pos:Int):FList[A]
  //Insertion sort
  def inSorted(f:A=>Int)(x:A):FList[A]

  //Sorting using insertion sort
  /*
  [("A",1), ("B",9), ("C",10)]
  java => comparator
  scala => sortWith
  sortBy(_._2) -second value from the pair
  sortBy(_._1.length) or sortBy(x=> x._2.length)
   */
  def sortBy(f:A=>Int):FList[A]
}

case class FNil[A]() extends FList[A]{
  override def length:Int = 0

  override def head: A = throw new Exception("head of empty list")

  override def map[B](f: A => B): FList[B] = FNil()

  override def foldRight[B](acc: B)(op: (B, A) => B): B = acc
}

case class Cons[A](x:A,xs: FList[A]) extends FList[A]{
  override def length: Int = 1 + xs.length

  override def head: A = x

  override def map[B](f: A => B): FList[B] = Cons(f(x), xs.map(f))

  override def foldRight[B](acc: A)(op: (A, B) => B): B =
    op(x,xs.foldRight(acc)(op))

  override foldLeft()

}