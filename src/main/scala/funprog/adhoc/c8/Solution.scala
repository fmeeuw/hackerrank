package funprog.adhoc.c8

import scala.annotation.tailrec
import scala.io.StdIn

/**
  * Mangoes
  * https://www.hackerrank.com/challenges/mango
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

  private def optimalFriendsToInvite(totalMangoes: Long, allFriends: List[Friend]): Long = {
    @tailrec
    def findOptimalFriends(invite: Int): Int = {
      if (invite > allFriends.size) allFriends.size
      else if (minMangoesEaten(allFriends, invite) <= totalMangoes) findOptimalFriends(invite + 1)
      else invite - 1
    }
    findOptimalFriends(1)
  }

  private def minMangoesEaten(allFriends: List[Friend], invite: Int): Long = {
    def mangoesEaten(friends: List[Friend]): Long = {
      friends
        .map(_.eatsMangoes(friends.length - 1))
        .sum
    }

    allFriends
      .combinations(invite)
      .map(mangoesEaten)
      .min
  }

}

