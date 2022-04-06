/*
Define the function foldWith which uses an operation op to reduce a range of integers to a value.
For instance, given that op is addition (+), the result of folding the range 1 to 3 will be 1+2+3=6.
foldWith should be curried (it will take the operation and return another function which expects
the bounds).
 */
def foldWith (op: (Int,Int) => Int)(start: Int, stop: Int): Int = {
  def tail_fold(crt: Int, acc: Int): Int  = {
    if(crt > stop) acc
    else tail_fold(crt+1,op(crt,acc))
  }
  tail_fold(start, 0)
}

foldWith((x:Int,y:Int) => x*y)(5,10)

/*
Define the function foldConditional which extends foldWith by also adding a predicate p: Int ⇒ Int.
foldConditional will reduce only those elements of a range which satisfy the predicate.
 */
def foldConditional(op: (Int,Int) => Int, p: Int => Boolean)(start: Int, stop: Int): Int = {
  def tail_fold(crt: Int, acc: Int): Int  = {
    if(crt > stop) acc
    else if (p(crt)) tail_fold(crt+1,op(crt,acc))
    else tail_fold((crt+1),op(0,acc))

  }
  tail_fold(start, 0)
}

foldConditional(((x:Int,y:Int) => x+y), ((z:Int) => z % 2 == 0))(1,4)

/*

 */

def foldMap(op: (Int,Int) => Int, f: Int => Int)(start: Int, stop: Int): Int = {
  def aux(i:Int, acc:Int):Int =
    if(i > stop) acc
    else aux(i+1,op(i,f(i)))
  aux(start,0)
}

foldMap((x:Int,y:Int) => x+y,((z:Int) => z*z))(0,5)


def sumSquares(n: Int): Int = {
  foldMap((x:Int,y:Int) => x+y,(n:Int) => n*n)(0,10)
}

/*
Write a function hasDivisor which checks if a range contains a multiple of k.
Use foldMap and choose f carefully.
 */

def hasDivisor(k: Int, start: Int, stop: Int): Boolean = foldMap(((x:Int,y:Int) => x+y),(m:Int) => if(m % k == 0) 1 else 0 )(0,20) > 0




/*
We can compute the sum of an area defined by a function within a range a,b
(the integral of that function given the range), using the following recursive scheme:
otherwise, we compute the mid of the range, we recursively compute the integral from a
to mid and from mid to b, and add-up the result.
Implement the function integral which computes the integral of a function f given a range:
 */
def integrate(f: Double => Double)(start: Double, stop: Double): Double = {
  if(stop-start < 0.1) (f(start) + f(stop))*((stop-start)/2)
  else (integrate(f)(start, (start+stop)/2) + integrate(f)((start+stop)/2,stop)