trait Nat {}
case object Zero extends Nat {}
case class Succ(n: Nat) extends Nat {}

def add(n: Nat, m: Nat): Nat ={
  n match {
    case Zero => Zero
    case Succ(n) => Succ(add(n,m))
  }
}

def toInt(n: Nat): Int = {
  n match {
    case Zero => 0
    case Succ(n) => 1 + toInt(n)
  }
}

def fromInt(i: Int): Nat = {
  i match {
    case 0 => Zero
    case i => Succ(fromInt(i-1))
  }
}


trait ITree {}
case object Empty extends ITree
case class INode(key: Int, left: ITree, right: ITree) extends ITree

val tree = INode(5,
                 INode(2,INode(1,Empty,Empty),INode(3,Empty,Empty)),
                 INode(7,Empty,INode(9,Empty,Empty)))

def size(t:ITree):Int =
  t match {
    case Empty => 0
    case INode(_,left,right) => 1 + size(left) + size(right)
  }

def contains(t:ITree, x:Int):Boolean =
  t match {
    case Empty => false
    case INode(key, left, right) => if (x == key) true
                                    else if(x > left) contains(left, x)
                                    else contains(right, x)
  }

def ins(t:ITree, x:Int):ITree = {
  t match {
    case Empty => INode(x,Empty,Empty) // Empty???
    case INode(key,left,right) => if(x < key) INode(x,left,ins(t,x))
                                  else if (x > key) INode(key,ins(t,x),right)
                                  else INode(x,Empty,Empty)
  }
}

def flatten(t:ITree):List[Int]= {
  t match {
    case Empty =>Nil
    case INode(k, l, r) => flatten(l):::(k::flatten(r))
  }

}

// List(1,2,3):::List(4,5).