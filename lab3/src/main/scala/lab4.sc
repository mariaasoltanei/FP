import scala.::
import scala.collection.immutable

def atLeastk(k: Int, l: List[Int]): Boolean =
  if (k == 0) false
  else l match {
    case x :: xs => if(k == x) true else atLeastk(k,xs)
  }

atLeastk(0,List(1,2,3,6,47,9))

//Write a function which returns
// the first n elements from a given list.
def take(n: Int, l: List[Int]): List[Int] = {
  if (n == 0) Nil
  else
  l match {
    case Nil => Nil
    case x :: xs => take(n-1,xs)
  }

}
take(3,List(1,2,3,4,5))

//Write a function which drops the first n elements from a given list.
def drop(n: Int, l: List[Int]): List[Int] = {
  if (n == 0) l
  else
  l match {
    case Nil => l
    case x :: xs => drop(n-1,xs)
  }
}
drop(3,List(1,2,3,4,5))

//returns a sublist of l containing those elements for which p is true
def takeP(p: Int => Boolean)(l: List[Int]): List[Int] = {
  l match {
    case Nil => l
    case x :: xs => if(p(x)) x::takeP(p)(xs) else takeP(p)(xs)
  }
}
takeP(_%2 == 0)(List(1,2,3,4,5,6))

//Write a function which uses a predicate to partition (split) a list.
def part(p: Int => Boolean)(l: List[Int]): (List[Int], List[Int]) = {
  l match {
    case Nil => (l,l)
    case x::xs => {
      val aux = part(p)(xs)
      if(p(x)) (x::aux._1,aux._2) else (aux._1,x::aux._2)
    }
  }
}
part(_%2 == 0)(List(1,2,3,4,5,6))

val gradebook = List(("G",3), ("F", 10), ("M",6), ("P",4))
type Gradebook = List[(String,Int)]

//adds one point to all students which have a passing grade (>= 5), and leaves all other grades unchanged.
//

def increment(g: Gradebook): Gradebook = {
  g.map(p => if(p._2 >= 5) (p._1,p._2 +1) else (p._1,p._2))

}
