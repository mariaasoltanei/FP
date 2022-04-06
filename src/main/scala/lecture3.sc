import jdk.javadoc.internal.tool.Start

def sumOf(start:Int, stop:Int): Int = {
  def auxSum(i:Int, acc:Int): Int =
    if (i>stop) acc
    else auxSum(i+1,i+acc)
  auxSum(start, 0)
}

def squaresOf(start:Int, stop:Int): Int = {
  def aux(i:Int, acc:Int): Int =
    if (i>stop) acc
    else aux(i+1,i*i+acc)
  aux(start, 0)
}

def sumWithf(f:Int => Int, start: Int, stop:Int): Int = {
  def aux(i:Int, acc:Int): Int =
    if (i>stop) acc
    else aux(i+1,f(i)+acc)
  aux(start, 0)
}

def id(x:Int): Int = x
def square(x:Int): Int = x*x

sumWithf((x:Int) => x,0,10) //or we can ommit Int
sumWithf((x:Int) => x*x, 0,10)

(x:Int, y:Int) => x + y
(x:Boolean,y:Boolean) => x && y
//or
val f :(Boolean,Boolean) => Boolean = (x,y) => x && y



def withf(f:Int => Int) : (Int,Int) => Int = {
  def sumf(start:Int, stop:Int): Int = {
    def aux(i:Int, acc:Int):Int =
      if(i > stop) acc
      else aux(i+1,f(i)+acc)
    aux(start,0)
  }
  sumf //returning, not calling
}
//preffered syntax
def withfp(f:Int => Int)(start:Int, stop:Int) : Int = {
  def aux(i:Int, acc:Int):Int =
    if(i > stop) acc
    else aux(i+1,f(i)+acc)
  aux(start,0)
}

withfp(x =>x)(0,10)
def curryf(x:Int)(y:Int)(z:Int): Int = x+y+z

val sum: (Int,Int) => Int = withfp(x =>x)
sum(0,10)
sum(10,20)

val squares: (Int,Int) => Int = withfp(x => x*x)
squares(1,5)

def uncurryf(x:Int, y:Int, z:Int): Int = x+y+z

def separation(reduce:(Int,Int) => Int):Int = { //reduce-function takes a range, returns a value
  def found(value:Int):Boolean = value % 2 == 0 //property we need to check
  def search(x:Int):Int = {
    val v = reduce(x,x+10)
    if(found(v)) x
    else search(x+1)
  }
  search(0)
}

separation(withfp(x=>x))
