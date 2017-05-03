package funprog.adhoc.c8

import scala.annotation.tailrec
import scala.collection.SeqView
import scala.io.StdIn

/**
  * Mangoes
  * https://www.hackerrank.com/challenges/mango
  *
  * After initial slow solution improved performance by:
  * - Using binary search to find the optimal number of friends, instead of simply starting from 1 upwards.
  * - Instead of looking at all combinations of friends, only select the combination that easts the least mangoes.
  *
  * @author Frank van Meeuwen
  */
object Solution {

  case class Friend(appetite: Long, happiness: Long) {
    def eatsMangoes(otherFriends: Long): Long = {
      appetite + (otherFriends * happiness)
    }
  }

  def main(args: Array[String]): Unit = {
    val _::nrOfMangoes::Nil = readLongs()
    val appetites = readLongs()
    val happiness = readLongs()
    val friends = appetites.zip(happiness).map { case (a, h) => Friend(a,h) }
    println(optimalFriendsToInvite(nrOfMangoes, friends))
  }

  private def readLongs(): List[Long] = {
    StdIn.readLine().split(" ").map(_.toLong).toList
  }

  /**
    * Applies binary search to find the index of the greatest element in the list <= max.
    */
  private def binarySearchMax(elements: Seq[Long], max: Long): Option[Int] = {
    @tailrec
    def searchMax(prevIdx: Int, idx: Int): Option[Int] = {
      // Next step is the difference of previous index and this index, divided by two, but at least 1.
      val nextStep = Math.max(1, Math.abs(idx - prevIdx) / 2)
      val nextIdx = elements(idx).compareTo(max) match {
        case x if x <= 0 => idx + nextStep
        case x if x > 0 => idx - nextStep
      }

      //The stopping conditions:
      // - The nextIdx is out of bounds
      // - The nextIdx is allready visited.
      if(nextIdx > elements.size - 1) Some(elements.size - 1)
      else if (nextIdx < 0) None
      else if (nextIdx == prevIdx) {
        if(nextIdx >= idx) Some(idx)
        else Some(nextIdx)
      }
      else {
        searchMax(idx, nextIdx)
      }
    }
    searchMax(0,elements.size/2)
  }

  private def optimalFriendsToInvite(totalMangoes: Long, allFriends: Seq[Friend]): Long = {
    val minMangoesEatenPerInvite: SeqView[Long, Seq[_]] = (1 to allFriends.size).view.map(invites => minMangoesEaten(allFriends, invites))
    binarySearchMax(minMangoesEatenPerInvite, totalMangoes) match {
      case Some(idx) => idx+1
      case None => 0
    }
  }

  private def minMangoesEaten(allFriends: Seq[Friend], invite: Int): Long = {
    allFriends
      .map(_.eatsMangoes(invite - 1))
      .sorted
      .take(invite)
      .sum
  }

}

