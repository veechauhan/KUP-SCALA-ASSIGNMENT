package edu.knoldus

import scala.annotation.tailrec

class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {
    //    for (j <- 1 until array.length) {
    //      breakable {
    //        for (i <- (1 to j).reverse) {
    //          if (array(i - 1) < array(i)) {
    //            break
    //          } else {
    //            val temp = array(i)
    //            array(i) = array(i - 1)
    //            array(i - 1) = temp
    //          }
    //        }
    //      }
    //    }
    //    arrayii
    val list: List[Int] = array.toList

    def helperInsertionSort(list: List[Int]): List[Int] =
      if (list.isEmpty) {
        Nil
      } else {
        insert(list.head, helperInsertionSort(list.tail))
      }

    def insert(x: Int, list: List[Int]): List[Int] =
      if (list.isEmpty || x <= list.head) {
        x :: list
      } else {
        list.head :: insert(x, list.tail)
      }

    helperInsertionSort(list).toArray
  }

  def selectionSort(array: Array[Int]): Array[Int] = {

    val list: List[Int] = array.toList

    @tailrec
    def selectSortHelper(list: List[Int], accumList: List[Int] = List[Int]()): List[Int] = {

      list match {
        case Nil => accumList
        case _ => {
          val min = list.min
          val requiredList = list.filter(_ != min)
          selectSortHelper(requiredList, accumList ::: List.fill(list.length - requiredList.length)(min))
        }
      }
    }

    selectSortHelper(list).toArray
  }


  def bubbleSort(array: Array[Int]): Array[Int] = {

    def helperBubbleSort(array: Array[Int], len: Int): Int = {
      if (len == 1) {
        return 0
      }
      for (i <- 0 until len - 1) {
        if (array(i) > array(i + 1)) {
          val temp = array(i)
          array(i) = array(i + 1)
          array(i + 1) = temp
        }
      }
      helperBubbleSort(array, len - 1)
    }

    helperBubbleSort(array, array.length)
    array
  }
}

//object Main {
//  def main(args: Array[String]): Unit = {
//    val obj = new Sorting
//    //    obj.insertionSort(Array(5, 4, 3, 2, 2, 1)) foreach print
//    println
//    obj.bubbleSort(Array(5, 4, 3, 1, 2)) foreach print
//    println
//    //    obj.selectionSort(Array(5, 4, 3, 1, 2)) foreach print
//
//
//  }
//}