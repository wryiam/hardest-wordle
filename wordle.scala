// Main Part 2 about Evil Wordle
//===============================


object M2 {

  import io.Source
  import scala.util._

  // ADD YOUR CODE BELOW
  //======================


  //(1)
  def get_wordle_list(url: String): List[String] = {
    Try {
      Source.fromURL(url)("UTF-8").getLines().toList
    }.getOrElse(Nil)
  }

  // val secrets = get_wordle_list("https://nms.kcl.ac.uk/christian.urban/wordle.txt")
  // secrets.length // => 12972
  // secrets.filter(_.length != 5) // => Nil

  //(2)
  def removeN[A](xs: List[A], elem: A, n: Int): List[A] = {
    if (xs.isEmpty) Nil
    else {
      val head = xs.head
      val tail = xs.tail
      if (head == elem && n > 0) removeN(tail, elem, n - 1)
      else head :: removeN(tail, elem, n)
    }
  }


  // removeN(List(1,2,3,2,1), 3, 1)  // => List(1, 2, 2, 1)
  // removeN(List(1,2,3,2,1), 2, 1)  // => List(1, 3, 2, 1)
  // removeN(List(1,2,3,2,1), 1, 1)  // => List(2, 3, 2, 1)
  // removeN(List(1,2,3,2,1), 0, 2)  // => List(1, 2, 3, 2, 1)

  // (3)
  abstract class Tip

  case object Absent extends Tip

  case object Present extends Tip

  case object Correct extends Tip


  def pool(secret: String, word: String): List[Char] = {
    (secret zip word).collect {
      case (s, w) if s != w => s
    }.toList
  }

  def aux(secret: List[Char], word: List[Char], pool: List[Char]): List[Tip] = {
    (secret, word) match {
      case (Nil, Nil) => Nil
      case (secHead :: secTail, wordHead :: wordTail) =>
        if (secHead == wordHead) Correct :: aux(secTail, wordTail, pool)
        else if (pool.contains(wordHead)) Present :: aux(secTail, wordTail, pool.diff(List(wordHead)))
        else Absent :: aux(secTail, wordTail, pool)
    }
  }


  def score(secret: String, word: String): List[Tip] = {
    aux(secret.toList, word.toList, pool(secret, word))
  }


  // score("chess", "caves") // => List(Correct, Absent, Absent, Present, Correct)
  // score("doses", "slide") // => List(Present, Absent, Absent, Present, Present)
  // score("chess", "swiss") // => List(Absent, Absent, Absent, Correct, Correct)
  // score("chess", "eexss") // => List(Present, Absent, Absent, Correct, Correct)

  // (4)
  def eval(t: Tip): Int = t match {
    case Correct => 10
    case Present => 1
    case Absent => 0
  }

  def iscore(secret: String, word: String): Int = {
    score(secret, word).map(eval).sum
  }

  //iscore("chess", "caves") // => 21
  //iscore("chess", "swiss") // => 20

  // (5)
  def lowest(secrets: List[String], word: String, current: Int, acc: List[String]): List[String] = {
    secrets match {
      case Nil => acc
      case head :: tail =>
        val score = iscore(head, word)
        if (score < current) lowest(tail, word, score, List(head))
        else if (score == current) lowest(tail, word, current, acc :+ head)
        else lowest(tail, word, current, acc)
    }
  }

  def evil(secrets: List[String], word: String): List[String] = {
    lowest(secrets, word, Int.MaxValue, Nil)
  }


  //evil(secrets, "stent").length
  //evil(secrets, "hexes").length
  //evil(secrets, "horse").length
  //evil(secrets, "hoise").length
  //evil(secrets, "house").length

  // (6)
  def frequencies(secrets: List[String]): Map[Char, Double] = {
    val totalLetters = secrets.length * 5
    val allCharacters = secrets.flatMap(_.toList)
    val characterCounts = allCharacters.groupBy(char => char).view.mapValues(_.size).toMap
    val frequencies = ('a' to 'z').map { char =>
      val count = characterCounts.getOrElse(char, 0)
      val frequency = 1.0 - count.toDouble / totalLetters
      char -> frequency
    }.toMap
    frequencies
  }

  // (7)
  def rank(frqs: Map[Char, Double], s: String): Double = {
    if (s.isEmpty) 0.0
    else frqs.getOrElse(s.head, 0.0) + rank(frqs, s.tail)
  }

  def ranked_evil(secrets: List[String], word: String): List[String] = {
    val freqMap = frequencies(secrets)
    val evilWords = evil(secrets, word)
    val rankedWords = evilWords.map(word => word -> rank(freqMap, word)).toMap
    val maxRank = rankedWords.values.max
    rankedWords.filter(_._2 == maxRank).keys.toList
  }
}


// William Costales k23081539



