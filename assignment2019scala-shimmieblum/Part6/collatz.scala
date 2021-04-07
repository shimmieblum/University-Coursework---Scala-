// Basic Part about the 3n+1 conjecture
//======================================

object CW6a {

//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed, you can use an auxiliary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.

def step(n: Long) : Long = {
    if(n == 1) 0
    else if(n%2 == 0) 1 + step(n/2)
    else 1+ step(3*n +1)    
}

def collatz(n: Long) : Long = step(n)

//(2) Complete the collatz_max function below. It should
//    calculate how many steps are needed for each number 
//    from 1 up to a bound and then calculate the maximum number of
//    steps and the corresponding number that needs that many 
//    steps. Again, you should expect bounds in the range of 1
//    up to 1 Million. The first component of the pair is
//    the maximum number of steps and the second is the 
//    corresponding number.

def collatz_max(bnd:Long) : (Long, Long) = {
    val list = List.range(1,bnd)

    val max = list.maxBy(x=>collatz(x))
    (collatz(max),max)
}

}

