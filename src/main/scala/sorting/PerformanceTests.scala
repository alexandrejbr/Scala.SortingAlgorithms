package sorting

import scala.util.Random

object PerformanceTests {

  implicit def IntIntLessThan(x: Int, y: Int) = x < y

  def main(args: Array[String]) = {

    val r = randomList(new Random(), 4100)

    println("TailRecursiveMergeSort:" + benchmark(() => TailRecursiveMergeSort(r)))
    println("MergeSort:" + benchmark(() => MergeSort(r)))
    println("TailRecursiveMergeSortBad:" + benchmark(() => TailRecursiveMergeSortBad(r)))
    println("Default sort:" + benchmark(() => r.sorted))
  }
  
  def benchmark(ls: () => Unit): Long = {
    // warm up
    for(i <- 1 to 100){
      ls()
    }
    
    val initialTime = System.nanoTime()
    
    ls()
    
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