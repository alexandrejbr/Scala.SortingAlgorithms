package sorting

import scala.util.Random

object PerformanceTests {

  implicit def IntIntLessThan(x: Int, y: Int) = x < y

  def main(args: Array[String]) = {

    val r = randomList(new Random(), 8500)

    println("TailRecursiveMergeSort:" + benchmark(r, TailRecursiveMergeSort))
    println("MergeSort:" + benchmark(r, MergeSort))
    println("TailRecursiveMergeSortBad:" + benchmark(r, TailRecursiveMergeSortBad))    
  }
  
  def benchmark(original: List[Int], ls: ListSort): Long = {
    // warm up
    ls(original)
    
    val initialTime = System.nanoTime()
    
    ls(original)
    
    System.nanoTime - initialTime
  }

  def randomList(r: Random, size: Int): List[Int] = {
    def loop(r: Random, size: Int, acc: List[Int]): List[Int] =
      if (size == 0) acc else loop(r, size - 1, r.nextInt :: acc)
    loop(r, size - 1, List(r.nextInt))
  }

  def randomListR(r: Random, size: Int): List[Int] =
    if (size == 0) Nil else r.nextInt :: randomListR(r, size - 1)
}