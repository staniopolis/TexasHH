package game

import scala.io.StdIn

object Game extends App {

  type Card = (String, Int)
  type Block = List[Card]
  type Suit = Char
  type CardDeck = Map[Suit, Set[(Char, Int)]]
  var input: String = ""

  do {
    try {
      println("\nPlease, input card combination: 5 cards for board and 2 for each hand.\n" +
        "To exit game, please, press \"Enter\": \n")

      input = StdIn.readLine()
      if (input == "") {
        println("Thank you for playing. Have a nice day.")
        System.exit(0)
      }

      def validateInput(input: String) =
        if (input.length > 10 && input.split(" ").toList.length == 1) throw ARowOfSymbols()
        else if (input.split(" ").toList.head.length != 10) throw BoardCards()
        else if (input.split(" ").toList.length <= 2) throw NotEnoughParticipants()
        else if (input.split(" ").toList.tail.length > 23) throw ParticipantsNumberExceed()
        else if ((for (cards <- input.split(" ").toList.tail;
                       if cards.length != 4) yield false).contains(false)) throw HandCards()

        else true

      validateInput(input)

      val cardValues = Set(('A', 14), ('K', 13), ('Q', 12), ('J', 11), ('T', 10),
        ('9', 9), ('8', 8), ('7', 7), ('6', 6), ('5', 5), ('4', 4), ('3', 3), ('2', 2))
      val suits = List('h', 'd', 'c', 's')
      val cardDeck: Map[Suit, Set[(Char, Int)]] = (for (s <- suits) yield s -> cardValues).toMap


      val gameParticipants: List[String] = input.split(" ").toList

      def splitCards(str: String, acc: List[String]): List[String] =
        if (str.isEmpty) acc
        else splitCards(str.splitAt(2)._2, acc :+ str.splitAt(2)._1)

      def validateCards(cards: List[String], cardDeck: CardDeck): Boolean =
        if (cards.isEmpty) true
        else
          validateCards(cards.tail, cardDeck.get(cards.head.last) match {
            case Some(list) => if (list.map(_._1).contains(cards.head.head))
              cardDeck.updated(cards.head.last, list.filter(elem => elem._1 != cards.head.head))
            else throw NotACard(cards.head)
            case None => throw NotACard(cards.head)
          })

      validateCards(splitCards(input.replaceAll(" ", ""), List()), cardDeck)

      def cardsByValue(cards: List[String], acc: Block): Block =
        if (cards.isEmpty) acc
        else cardsByValue(cards.tail, acc :+ (cards.head, cardValues.filter(x => x._1 == cards.head.head).head._2))

      val board: String = gameParticipants.head
      val boardCards: List[String] = splitCards(board, List())
      val boardCardsByValue: Block = cardsByValue(boardCards, List())

      def splitCardsLoop(handsList: List[String], acc: List[List[String]]): List[List[String]] =
        if (handsList.isEmpty) acc
        else splitCardsLoop(handsList.tail, acc :+ splitCards(handsList.head, List()))

      def cardsByValueLoop(handsCards: List[List[String]], acc: List[Block]): List[Block] =
        if (handsCards.isEmpty) acc
        else cardsByValueLoop(handsCards.tail, acc :+ cardsByValue(handsCards.head, List()))

      val hands: List[String] = gameParticipants.tail
      val handsCards: List[List[String]] = splitCardsLoop(hands, List())
      val handsCardsByValue: List[Block] = cardsByValueLoop(handsCards, List())


      def createHands(inputHands: List[Block]): List[HandBlock] =
        if (inputHands.isEmpty) List()
        else new HandBlock(inputHands.head, boardCardsByValue) :: createHands(inputHands.tail)

      val handBlocks = createHands(handsCardsByValue)


      val sorted = handBlocks.sortBy(x => (x.finalRank._2, x.finalRank._3, x.finalRank._4))

      def outputString(result: List[HandBlock]): String = {
        if (result.tail.isEmpty) s"${result.head.hand.map(_._1).mkString}"
        else if (result.head.finalRank._2 == result.tail.head.finalRank._2
          && result.head.finalRank._3 == result.tail.head.finalRank._3
          && result.head.finalRank._4 == result.tail.head.finalRank._4) s"${result.head.hand.map(_._1).mkString}=" +
          outputString(result.tail)
        else s"${result.head.hand.map(_._1).mkString} " + outputString(result.tail)
      }

      println(outputString(sorted))
    } catch {
      case e: ARowOfSymbols => println("WARNING: This is just a row of symbols")
      case e: ParticipantsNumberExceed => println("WARNING: Participants number exceeded. Max: 23 hands and 1 board ")
      case e: NotEnoughParticipants => println("WARNING: Must be at least 2 participants")
      case e: BoardCards => println("WARNING: Board must contain 5 cards")
      case e: HandCards => println("WARNING: Each hand must contain 2 cards")
      case e: NotACard => println(s"WARNING: This -> ${e.card} <- is not a valid card or this card has already been dealt")
    }
  } while (true)

}

case class ParticipantsNumberExceed() extends Exception

case class NotEnoughParticipants() extends Exception

case class NotACard(card: String) extends Exception

case class ARowOfSymbols() extends Exception

case class BoardCards() extends Exception

case class HandCards() extends Exception
