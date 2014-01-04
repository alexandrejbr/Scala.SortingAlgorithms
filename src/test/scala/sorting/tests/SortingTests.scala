package sorting.tests

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import sorting._

@RunWith(classOf[JUnitRunner])
class SortingTests extends FunSuite {

  implicit def IntIntLessThan(x: Int, y: Int) = x < y
  
  def testSortingAlgorithm(original: List[Int], ls: ListSort) = {
    ls(original) === original.sorted
    
  }
  
  test("merge sort") {
    assert(testSortingAlgorithm(List(9,2,6,7,3,1), MergeSort))
  }
  
  test("merge sort tail recursive"){
    assert(testSortingAlgorithm(List(9,2,6,7,3,1), TailRecursiveMergeSortBad))
  }
  
  test("merge sort tail recursive v2"){
    assert(testSortingAlgorithm(List(9,2,6,7,3,1), TailRecursiveMergeSort))
  }
  
}