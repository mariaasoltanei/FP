/*
how to construct lists
 */
val l = List(1,2,3) //has type List[Int]
List("apples", "bananas") // has type List[String]
List('c','a','b') //List[Char]
List(List(1,2), List(3,4), List(7,8)) //has type List[List[Int]'
/*
Lists are:
-homogenous-all elem should be the same type
 */

/*how to construct lists (using cons, and NIL)
cons operator - ::
e::l -e is current element and l is the rest of the list
 */
1::2::3::Nil //same as List[1,2,3]
1::(2::3::Nil) //
/*
Pairs
two values ("first") and ("second")
not necessarily of the same type
 */

val pair = (1,true)
pair._1
pair._2

/*
Observers for lists
 */
l.head //a "function" with 0 parameters
l.tail

(1,2) :: (3,4) :: Nil //lists of pairs has type List[(Int,Int)] , all pairs in list have to be the same
(1,true) :: (2,false) :: Nil // List[(Int, Boolean)]

/*
Implement stuff with lists
*/

def contains(e:Int, l:List[Int]):Boolean = {
  if(l.isEmpty) false
  else if(e == l.head) true
  else contains(e,l.tail)
}
/*
there is a better way to decompose lists
Pattern matching
<expression> match {
  case <pattern1> =>
  case <pattern2> =>
  }

  What patterns can be:
  -patterns are "ways" to construct objects from their constructors

  Examples of patterns:
  x::Nil = the list with only one element
  List(x) - teh very same thing
  x::1::xs - a list of at least 2 elements
  Nil:xs -a list of lists where the first element is Nil
  (1::Nil)::xs - a list of lists where the first elem is [1]
  (a,b) :: xs -  a list of pairs, where the first pair is (a,b)
  (Nil,1::xs) - a pair where the first elem is the empty list and the second elem is a non empty list
   */

def contains1(e:Int, l:List[Int]):Boolean =
  l match {
    case Nil => false //is l empty?
    case x :: xs => if(e == x) true else contains1(e,xs)
  }

//add 1 to each element of the list
def addOne(l:List[Int]):List[Int] =
  l match {
    case Nil => Nil
    case x::xs => (x+1)::addOne(xs)
  }

def fmap(f:Int => Int)(l:List[Int]):List[Int] =
  l match {
    case Nil => Nil
    case x::xs => f(x) :: fmap(f)(xs)
  }

def multiplyBy(v:Int, l:List[Int]): List[Int] = fmap(_*v)(l)

/*
Anonymus functions - lambda (short version)
x => x*2 becomes _ * 2
(x,y) => x+y becomes _ + _
 */


val list1 = List(Nil, List(3,4), List(5,6))
val list2 = List(1,2,4)
list2.map(_  * 2)
list1.map(_.isEmpty)
list1.map(_.head)

def sum(l:List[Int]):Int =
  l match {
    case Nil => 0
    case x::xs => x+sum(xs)
  }

def product(l:List[Int]):Int =
  l match {
    case Nil => 1
    case x::xs => x*product(xs)
  }

/*
what is diff between sum and product:
the initial value (0 and 1)
teh operation
 */

def fold(acc:Int)(op: (Int, Int) => Int)(l:List[Int]) =
  l match {
    case Nil => acc
    case x::xs => op(x,fold(acc))(op)(xs)
  }

val list3 = List(1,2,3)
fold(0)(_+_)(list3)
fold(1)(_*_)(list3)

def max(l:List[Int]):Int =
  l.tail.foldRight(l.head)((x,crt_max) => if(x>crt_max) x else crt_max)


max(List(1,2,3,6,47,9))

def contains2(e:Int, l:List[Int]):Boolean =
  l.foldRight(false)((x,found) => if(x == e) true else found)
//x == e || found
//_ == e || _

contains2(5, List(1,3,4,5))

def avg(l:List[Int]):Int = {
  val p = l.foldRight((0,0))((x,pair) => (pair._1 + x, pair._2 + 1))
  p._1 / p._2
}
