/*
(Functional) Data representation  in Scala
Syntax basics
 */
 /*
 we def a class (called Point)
 we define the constructor for the class Point(1,2)
  */
class Point(x:Int,y:Int){
  //checks if a point is higher on the y axis than our current point
  def higher(p:Point):Boolean =
    p.y > this.y

  override def toString:String =
    s"({$x},{$y})"

}

val p = new Point(1,2)
p.higher(new Point(2,3))
new Point(2,1) //expression that evaluates to a number
new Point(2,1) == new Point(2,1) // evaluates to FALSE

//classes in scala are VERY similar to those in Java
/*
What we would not like in Java classes? -> changing "stuff" in an object (ex: not good to p.x = 9)
Case classes:
-single constructor
-parameters received at constructor are immutable
-objects themselves are immutable
-equality is established by construction
-we can use pattern matching
 */

/*
Traits in Scala are very similar to Java interfaces
 */

trait FTree {
  /*
  wHY DO WE NOT DEFINE METHODS SUCH AS isEmpty?
  it is possible =>> yes
   */
}

case object FEmpty extends FTree {}

case class FNode(key:Int, left: FTree, right:FTree) extends FTree {}

FEmpty == FEmpty //no "new" keyword

val t1 = FNode(7,
         FNode(1,FEmpty,FEmpty),
         FNode(2,FEmpty,FEmpty))

//we have created several identical object called FEmpty previously
//we would like to have a single object instead of 4
//we can use "object keyword


FNode(1,FEmpty,FEmpty) == FNode(1,FEmpty,FEmpty) //will return true

/*
 How do we decompose or "look into" objects> -> We use pattern matching
 */

def isEmpty(t:FTree):Boolean =
  t match {
    case FEmpty => true
    case FNode(_,_,_) => false
  }

def size(t:FTree):Int =
  t match {
    case FEmpty => 0
    case FNode(_,left,right) => 1 + size(left) + size(right)
  }

/*
Another implementation of Trees
which is structurally equiv, but the type is diff organized
 */

trait OOTree {
  def isEmpty:Boolean
  def size:Int
}

case object OOEmpty extends OOTree {
  override def isEmpty: Boolean = true

  override def size: Int = 0
}

case class OONode(key:Int,left:OOTree, right: OOTree) extends OOTree {
  override def isEmpty: Boolean = false

  override def size: Int = 1 + left.size + right.size
}

val t2 = OONode(7,
  OONode(1,OOEmpty,OOEmpty),
  OONode(2,OOEmpty,OOEmpty))

/*
what is the diff between FTree and OOTree
-isEmpty and size are members of OOTree while the same functions are just functions of FTree
-the first imp(FTree) relies on pattern matching to decompose the object
-the 2nd impl(OOTree) uses "methods"

FUNCTIONAL DECOMPOSITION (using patterns)
-> we use when and why?
  - the constructors are fixed or unlikely to change during the project
  - methods are likely to change
  -
Object Oriented decomposition
-> we use when and why?
  - new constructors may appear
  -the methods are fixed
 */

t2.isEmpty
t2.size

/*
Suppose we wold like to make a project regarding expression
an expression can be:
  - <expr> = <val> || <var>
           = <expr> + <expr>
           = <expr> * <expr>
           the method evals which evaluates expression
 */

trait Expr {
  def eval:Int
}

case class Val(i:Int) extends Expr {
  override def eval: Int = i
}

case class Add(left:Expr, right: Expr) extends Expr {
  override def eval: Int = left.eval + right.eval
}

case class Multiplication(left:Expr, right: Expr) extends Expr {
  override def eval: Int = left.eval * right.eval
}

val e = Add(Val(2),Multiplication(Val(3),Val(4)))
e.eval
