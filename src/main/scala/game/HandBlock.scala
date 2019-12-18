package game

class HandBlock(val hand: List[(String, Int)], boardCardsByValue: List[(String, Int)]) {

  type Card = (String, Int)
  type Hand = List[Card]

  //---------------------------------------------------------------------------------------------------------------
  val handWithBoard: List[Card] = boardCardsByValue ::: hand
  val handWithBoardSorted: List[Card] = handWithBoard.sortBy(_._2).reverse
  val groupedBySuit: List[List[Card]] = handWithBoard.groupBy(_._1.last).values.toList.sortBy(_.length).reverse
  val groupedByValueAndSortedByLengthAndValue: List[List[Card]] =
    handWithBoard.groupBy(x => x._1.head).values.toList.sortBy(x => (x.length, x.map(_._2).max)).reverse

  val finalRank: (String, Int, Int, Int) =
    if (isStraitFlush(isStraight(handWithBoardSorted), isFlush(groupedBySuit.head)).nonEmpty)
      ("StraightFlush", 8, isStraitFlush(isStraight(handWithBoardSorted), isFlush(groupedBySuit.head)).map(_._2).sum, 0)

    else if (fourOfKind(groupedByValueAndSortedByLengthAndValue).nonEmpty)
      ("FourOfKind", 7, fourOfKind(groupedByValueAndSortedByLengthAndValue).take(4).map(_._2).sum,
        fourOfKind(groupedByValueAndSortedByLengthAndValue).drop(4).map(_._2).max)

    else if (isFullHouse(groupedByValueAndSortedByLengthAndValue.head,
      groupedByValueAndSortedByLengthAndValue.tail.head).nonEmpty)
      ("FullHouse", 6, isFullHouse(groupedByValueAndSortedByLengthAndValue.head,
        groupedByValueAndSortedByLengthAndValue.tail.head).take(3).map(_._2).sum,
        isFullHouse(groupedByValueAndSortedByLengthAndValue.head,
          groupedByValueAndSortedByLengthAndValue.tail.head).drop(3).map(_._2).sum)

    else if (isFlush(groupedBySuit.head.take(5)).nonEmpty)
      ("Flush", 5, isFlush(groupedBySuit.head.take(5)).map(_._2).sum, 0)

    else if (isStraight(handWithBoardSorted.distinctBy(_._2)).nonEmpty)
      ("Straight", 4, isStraight(handWithBoardSorted.distinctBy(_._2)).map(_._2).sum, 0)

    else if (threeOfKind(groupedByValueAndSortedByLengthAndValue).nonEmpty)
      ("ThreeOfKind", 3, threeOfKind(groupedByValueAndSortedByLengthAndValue).take(3).map(_._2).sum,
        threeOfKind(groupedByValueAndSortedByLengthAndValue).drop(3).map(_._2).max)

    else if (twoPairs(groupedByValueAndSortedByLengthAndValue).nonEmpty)
      ("TwoPairs", 2, twoPairs(groupedByValueAndSortedByLengthAndValue).take(2).map(_._2).sum,
        twoPairs(groupedByValueAndSortedByLengthAndValue).drop(2).init.map(_._2).sum * 10 +
          twoPairs(groupedByValueAndSortedByLengthAndValue).last._2)

    else if (pair(groupedByValueAndSortedByLengthAndValue).nonEmpty)
      ("Pair", 1, pair(groupedByValueAndSortedByLengthAndValue).take(2).map(_._2).max,
        pair(groupedByValueAndSortedByLengthAndValue).drop(2).map(_._2).max)

    else ("HighCard", 0, handWithBoardSorted.take(5).map(_._2).max, 0)

  //------------------Straight-----------------------------------------------------------------------------
  def commonStraight(cards: List[Card]): Hand =
    if (cards.splitAt(4)._2.isEmpty) List()
    else if (cards.take(5).head._2 - cards.take(5).last._2 == 4) cards.take(5)
    else commonStraight(cards.tail)

  def uncommonStraight(cards: List[Card]): Hand =
    if (cards.drop(3).head._2 == 11) cards.take(4) :+ cards.last
    else if (cards.drop(2).head._2 == 12 && cards.init.last._2 == 3) cards.take(3) ::: cards.takeRight(2)
    else if (cards.tail.head._2 == 13 && cards.dropRight(2).last._2 == 4) cards.take(2) ::: cards.takeRight(3)
    else if (cards.dropRight(3).last._2 == 5) cards.head :: cards.takeRight(4)
    else List()

  def isStraight(cards: List[Card]): Hand =
    if (cards.length >= 5) commonStraight(cards) match {
      case x :: xs => x :: xs
      case Nil => if (cards.map(_._2).contains(2) &&
        cards.map(_._2).contains(14)) uncommonStraight(cards) else List()
    }
    else List()

  //------------------Flush---------------------------------------------------------------------------------

  def isFlush(cards: List[Card]): Hand =
    if (cards.length >= 5) cards.sortBy(_._2).reverse
    else List()


  //------------------Straight Flush------------------------------------------------------------------------------
  def isStraitFlush(strait: List[Card], flush: List[Card]): Hand =
    if (strait.nonEmpty && flush.nonEmpty) isStraight(groupedBySuit.head.sortBy(_._2).reverse)
    else List()


  //-------------------Full House-----------------------------------------------------------------------------------
  def isFullHouse(threeOfKind: List[Card], cards: List[Card]): Hand =
    if ((threeOfKind ::: cards.take(2)).length == 5) (threeOfKind ::: cards.take(2))
    else List()


  //--------------------Four king-----------------------------------------------------------------------------------
  def fourOfKind(cards: List[List[Card]]): Hand =
    if (cards.head.length == 4) cards.flatten
    else List()


  //--------------------Three Of Kind-------------------------------------------------------------------------------
  def threeOfKind(cards: List[List[Card]]): Hand =
    if (cards.head.length == 3) cards.flatten.take(5)
    else List()


  //-------------------Two Pairs-----------------------------------------------------
  def twoPairs(cards: List[List[Card]]): Hand =
    if (cards.head.length == 2 && cards.tail.head == 2) cards.flatten.take(5)
    else List()


  //-------------------Pair-----------------------------------------------------
  def pair(cards: List[List[Card]]): Hand =
    if (cards.head.length == 2) cards.flatten.take(5)
    else List()


  //------------------High card------------------------------------------------
  def highCard(cards: List[Card]): Hand = handWithBoardSorted.take(5)


}