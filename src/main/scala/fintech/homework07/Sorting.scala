package fintech.homework07

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Реализовать алгоритмы quick-sort и merge-sort
  *  использую *подходящие* *мутабельные* коллекции
  */

object Sorting {

  def mergeSort[T](inputList: ListBuffer[T])(implicit ordering: Ordering[T]): Unit = {

    def makeSorting(inputList: ListBuffer[T])(implicit ordering: Ordering[T]): ListBuffer[T] = {
      def merge(left: ListBuffer[T], right: ListBuffer[T]): ListBuffer[T] = {
        (left.length, right.length) match {
          case (0, _) => right
          case (_, 0) => left
          case (_, _) =>
            if (ordering.compare(left.head, right.head) <= 0)
              left.head +: merge(left.tail, right)
            else
              right.head +: merge(left, right.tail)
        }
      }

      val n = inputList.length / 2
      if (n == 0) inputList
      else {
        val (left, right) = inputList splitAt n
        merge(makeSorting(left), makeSorting(right))
      }
    }

    val sorted: ListBuffer[T] = makeSorting(inputList)
    inputList.clear()
    for (item <- sorted)
      inputList += item
  }

 def quickSort[T](inputList: mutable.IndexedSeq[T])(implicit ordering: Ordering[T]): Unit = {

     def makeQSorting(l: Int, r: Int)(implicit ordering: Ordering[T]): Unit = {
       if (l < r) {
         val q = partition(l, r)
         makeQSorting(l, q)
         makeQSorting(q + 1, r)
       }
     }

    def partition(l: Int, r: Int)(implicit ordering: Ordering[T]): Int = {
      val v = inputList((l + r) / 2)
      var i = l
      var j = r
      while (i <= j) {
        while (ordering.compare(inputList(i), v) < 0)
          i += 1
        while (ordering.compare(inputList(j), v) > 0)
          j -= 1
        if (i < j) {
          val tmp = inputList(i)
          inputList(i) = inputList(j)
          inputList(j) = tmp
          i += 1
          j -= 1
        }
        else
          i = j + 1
      }
      j
   }

   makeQSorting(0, inputList.length - 1)
 }
}
