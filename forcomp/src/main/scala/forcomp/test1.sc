//List(List((1,1),(2,1)), List((3,1),(4,2)),(2,3)).flatten
type Occurrences = List[(Char, Int)]

val x = List(('b',1),('c',2))
val y = List(('b',1),('c',1))
subtract(x,y)

combinations(List())
val z = List(('a', 2), ('b', 2))
combinations(z)

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  def substractTerm(xmap: Map[Char,Int], term:(Char,Int)) = {
    val (c, i) = term
    xmap + (c -> (xmap(c)-i))
  }

  (y foldLeft x.toMap)(substractTerm)
    .toList
    .filter(i=>i._2 > 0)
    .sortBy(i=>i._1)
}
/*  (for((c,i) <- y) {
    yield x takeWhile { case (xc, xi) => xc != c}
  }).flatten*/

/*def subtract(x: Occurrences, y: Occurrences): Occurrences = y match {
  case Nil => x
  case (yc, yi) :: ytail =>
    val (xprefix, xtail) = x span { case (xc, xi) => xc != yc }
    xprefix :::
}*/

def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
  case Nil => List(List())
  case (c, n) :: tail => {
    val tailCombinations = combinations(tail)
    (for (i <- 1 to n; o <- tailCombinations)
      yield (c, i) :: o).toList ::: tailCombinations
  }
}
