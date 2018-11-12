package fintech.homework07
import org.scalatest.{FlatSpec, Matchers}
import fintech.homework07.Sorting._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

class SortingSpec extends FlatSpec with Matchers {
  it should "merge sort ints correctly" in {
    val actual = ListBuffer(2, 1, 3, 7)
    mergeSort(actual)
    actual should be(ListBuffer(1, 2, 3, 7))
  }

  it should "merge sort strings correctly" in {
    val actual = ListBuffer("2", "1", "3", "7")
    mergeSort(actual)
    actual should be(ListBuffer("1", "2", "3", "7"))
  }

  it should "quick sort ints correctly" in {
    val actual = mutable.IndexedSeq(2, 1, 3, 7)
    quickSort(actual)
    actual should be(IndexedSeq(1, 2, 3, 7))
  }

  it should " quick sort strings correctly" in {
    val actual = mutable.IndexedSeq("2", "1", "3", "7")
    quickSort(actual)
    actual should be(IndexedSeq("1", "2", "3", "7"))
  }

 it should "quick sort with user's ordering correctly ints" in {
    implicit val intOrdering: Ordering[Int] = new Ordering[Int] {
      override def compare(a: Int, b: Int): Int = if (a > b) -1 else if (a < b) 1 else 0
    }

    val actual = mutable.IndexedSeq(2, 0, 3, -1, 1, 4)
    quickSort(actual)
    actual should be(IndexedSeq(4, 3, 2, 1, 0, -1))
  }

  it should "quick sort with user's ordering correctly strings" in {
    implicit val intOrdering: Ordering[String] = new Ordering[String] {
      override def compare(a: String, b: String): Int = if (a.length > b.length) 1 else if (a.length < b.length) -1 else 0
    }

    val actual = mutable.IndexedSeq("lala", "la", "a")
    quickSort(actual)
    actual should be(IndexedSeq("a", "la", "lala"))
  }

  it should "merge sort with user's ordering correctly ints" in {
    implicit val intOrdering: Ordering[Int] = new Ordering[Int] {
      override def compare(a: Int, b: Int): Int = if (a > b) -1 else if (a < b) 1 else 0
    }

    val actual = ListBuffer(2, 0, 3, -1, 1, 4)
    mergeSort(actual)
    actual should be(ListBuffer(4, 3, 2, 1, 0, -1))
  }

  it should "merge sort with user's ordering correctly strings" in {
    implicit val intOrdering: Ordering[String] = new Ordering[String] {
      override def compare(a: String, b: String): Int = if (a.length > b.length) 1 else if (a.length < b.length) -1 else 0
    }

    val actual = ListBuffer("lala", "la", "a")
    mergeSort(actual)
    actual should be(ListBuffer("a", "la", "lala"))
  }
}