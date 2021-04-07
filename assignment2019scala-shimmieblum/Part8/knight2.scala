// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object CW8b {


// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions



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

def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    if(dim*dim == path.length && all_next_moves(path.head).contains(path.last)) Option(path)
    else{
        val l = ordered_moves(dim, path, path.head)
        if(l.length == 0) None
        else first(l, (x: Pos) => first_closed_tour_heuristics(dim, x::path))
    }
}

// first_closed_tour_heuristics(6, List((3,3)))


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] ={
    if(path.length == dim*dim) Option(path)
    else{
        val l = ordered_moves(dim, path, path.head)
        if(l.length == 0) None
        else first(l, (x: Pos) => first_tour_heuristics(dim, x::path))
    }
}

// first_tour_heuristics(30, List((0,0)))

}
