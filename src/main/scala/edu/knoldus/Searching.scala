package edu.knoldus

import scala.annotation.tailrec

class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean = {

    @tailrec
    def recursiveBinarySearch(array: Array[Int], elem: Int, low: Int, high: Int): Int = {
      if (low > high) return -1
      val mid = low + (high - low) / 2
      array(mid) match {
        case i if (i == elem) => mid
        case i if (i > elem) => recursiveBinarySearch(array, elem, low, mid - 1)
        case _ => recursiveBinarySearch(array, elem, mid + 1, high)

      }
    }

    val output = recursiveBinarySearch(array, elem, 0, array.length - 1)
    if (output == (-1)) {
      false
    } else {
      true
    }

  }

  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    if (array.isEmpty) {
      false
    } else if (array.head == elem) {
      true
    } else {
      linearSearch(array.tail, elem)
    }
  }

}
