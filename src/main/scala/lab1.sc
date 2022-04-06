def myAddition(x: Int, y:Int):Int = x+y
myAddition(1,2)
def inRange1(start:Int, stop:Int, x:Int):Boolean =
  if(x <=stop && x >=start) true
  else false
def inRange2(start:Int, stop:Int, x:Int):Boolean =
  x<=stop && x>=start

inRange1(1,5,3)
inRange1(2,7,3)

def signum(x:Int):Int =
  if (x == 0 ) 0
  else if(x>0) 1
  else -1

def and(x:Boolean, y:Boolean): Boolean =
  if(x) y else false

and(false,true)

def of(x:Boolean, y:Boolean):Boolean =
  if(x) true else y

def fact(n:Int): Int =
  if(n==1) 1
  else n*fact(n-1)

def fibon(n:Int):Int = {
  if(n == 0 ) 0
  else if(n==1) 1
  else fibon(n-1) +fibon(n-2)
}

def fibo1(n:Int): Int = {
  //local function definition
  def fibAux(n:Int,    //nth fibonacii number
             last:Int,  // the last fibo number
             beforeLast:Int //the previous one
            ): Int = {
    if( n == 0 ) beforeLast
    else fibAux(n-1, last + beforeLast, last)
    //the code block evaluates to the function call below
    fibAux(n, 1, 0)
  }

}

