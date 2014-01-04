package sorting

trait ListSort {
  //  implicit def IntIntLessThan(x: Int, y: Int) = x < y

  def apply[T](xs: List[T])(implicit less: (T, T) => Boolean): List[T]
}

object MergeSort extends ListSort {
  override def apply[T](xs: List[T])(implicit less: (T, T) => Boolean): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (less(x, y))
            x :: merge(xs1, ys)
          else
            y :: merge(xs, ys1)
      }
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(apply(ys), apply(zs))
    }
  }
}

object TailRecursiveMergeSortBad extends ListSort {
  override def apply[T](xs: List[T])(implicit less: (T, T) => Boolean): List[T] = {
    val m = xs.length / 2
    if (m == 0) xs
    else {
      @scala.annotation.tailrec
      def merge(ls: List[T], rs: List[T], acc: List[T] = List()): List[T] = (ls, rs) match {
        case (Nil, _) => acc ++ rs
        case (_, Nil) => acc ++ ls
        case (l :: ls1, r :: rs1) =>
          if (less(l, r)) merge(ls1, rs, acc :+ l)
          else merge(ls, rs1, acc :+ r)
      }
      val (l, r) = xs splitAt m
      merge(apply(l), apply(r))
    }
  }
}

object TailRecursiveMergeSort extends ListSort {
  def merge[T](xs: List[T], ys: List[T], res: List[T])(implicit less: (T, T) => Boolean): List[T] =
    (xs, ys) match {
      case (Nil, ys) => appendReversed(res, ys)//res.reverse ++ ys
      case (xs, Nil) => appendReversed(res, xs)//res.reverse ++ xs
      case (x :: xs1, y :: ys1) =>
        if (less(x, y)) merge(xs1, ys, x :: res)
        else merge(xs, ys1, y ::res)
    }

  def apply[T](xs: List[T])(implicit less: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0)
      xs
    else {
      val (left, right) = xs splitAt (n)
      merge(apply(left), apply(right), Nil)
    }
  }

  def appendReversed[T](left: List[T], right: List[T]): List[T] = left match {
    case Nil => right
    case head :: tail => appendReversed(tail, head :: right)
  }
}