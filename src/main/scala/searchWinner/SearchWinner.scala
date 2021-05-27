package searchWinner

import cats.{Monad, Parallel}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, Sync}
import cats.implicits.catsSyntaxParallelSequence
import dataBase.RequestInDB.writeGameCombination
import doobie.Transactor
import gameData.CardManipulation.numberNotEqualCard
import gameData.GameData._
import gameData.RefactorFunction._
import searchWinner.CheckCombination._
import doobie.implicits._
import cats.syntax.all._

import scala.concurrent.ExecutionContext
import cats.effect.ContextShift
import cats.instances.unit

object SearchWinner {
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  def searchCombination[F[_]: IO](
    listPlayers: List[PlayerDB],
    connectToDataBase: Transactor[F]
  )(implicit parallel: Parallel[F], sy: Sync[F]): F[Unit] =
    listPlayers match {
      case listPlayer if (listPlayers.size > 0) => {
        val player = PlayerFromPlayerDB(
          listPlayers.headOption.getOrElse(PlayerDB())
        )
        player match {
          case player
              if (
                (player.playerCard ++ player.allCard).size == 9 &&
                  ((player.playerCard ++ player.allCard)
                    .contains(numberNotEqualCard) == false)
              ) => {
            val playerRefIO: F[Ref[F, Player]] = Ref.of[F, Player](player)
            for {
              playerRef <- playerRefIO
              _ <- List(
                streetFlushRef(playerRef),
                fourCardsRef(playerRef),
                fullHouseRef(playerRef),
                flushRef(playerRef),
                streetRef(playerRef),
                setRef(playerRef),
                oneOrTwoPairRef(playerRef),
                highCardRef(playerRef)
              ).parSequence.void
              player               <- playerRef.get
              stringCombinationCard = listToString(player.cardForCombination)
              _                     = println(player)
              _ <- writeGameCombination(
                player.playerID,
                stringCombinationCard,
                player.combination
              ).transact(connectToDataBase)
              _ <- searchCombination(listPlayers.drop(1), connectToDataBase)
            } yield ()
          }
          case _ => _
        }
      }
      case _ => _
    }

  def searchWinner(listPlayer: List[Player]) = {
    listPlayer
      .sortBy(player =>
        (
          player.combination,
          player.cardForCombination.lift(0).getOrElse(-1),
          player.cardForCombination.lift(1).getOrElse(-1),
          player.cardForCombination.lift(2).getOrElse(-1),
          player.cardForCombination.lift(3).getOrElse(-1),
          player.cardForCombination.lift(4).getOrElse(-1)
        )
      )
      .reverse
  }

}
