// Finding a single tour on a "mega" board
//=========================================

object CW8c {

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

import scala.annotation.tailrec


type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.


def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
    x._1 < dim && x._1 >= 0 && x._2 < dim && x._2 >=0 && !path.contains(x)
}

def all_next_moves(x: Pos):List[Pos] = List((x._1+1,x._2+2),(x._1+2,x._2+1), (x._1+2,x._2-1) 
,(x._1+1,x._2-2), (x._1-1,x._2-2), (x._1-2,x._2-1), (x._1-2,x._2+1), (x._1-1,x._2+2))

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    all_next_moves(x).filter(x=>is_legal(dim,path,x))
}

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    legal_moves(dim,path,x).map(x=>(x,legal_moves(dim, path:+x, x).length)).sortBy(x =>x._2).map(x=>x._1) 
}


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 


def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
    if(xs.isEmpty) None
    else{
      f(xs.head) match{
        case None => first(xs.tail, f)
        case Some(value) => Option(value)
        }
    }
}


def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
    if(path.length == dim*dim) Option(path)
    else{
        val l = ordered_moves(dim, path, path.head)
        if(l.length == 0) None
        else first(l, (x: Pos) => tour_on_mega_board(dim, x::path))
    }
}
// def time_needed[T](code: => T) : T = {
//     val start = System.nanoTime()
//     val result = code
//     val end = System.nanoTime()
//     println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
//     result
//   }

// time_needed(tour_on_mega_board(70, List((0,0))))
    
}