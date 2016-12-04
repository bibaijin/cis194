{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spring13.Week12.Risk where

import           Control.Monad
import           Control.Monad.Random
import           Data.List

main :: IO ()
main = do
  b <- evalRandIO (successProb (Battlefield 10 10))
  print b

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield =
  Battlefield {attackers :: Army
              ,defenders :: Army}
  deriving ((Show))

------------------------------------------------------------
battle :: Battlefield -> Rand StdGen Battlefield

battle bf =
  dice (attackersInOneBattle + defendersInOneBattle) >>=
  \rands ->
    return $
    battleOutcome bf
                  (tally rands (attackersInOneBattle,defendersInOneBattle))
  where (attackersInOneBattle,defendersInOneBattle) = getArmiesInOneBattle bf

getArmiesInOneBattle :: Battlefield -> (Army, Army)
getArmiesInOneBattle bf = (attackersInOneBattle,defendersInOneBattle)
  where attackersInOneBattle =
          if attackers bf > 3
             then 3
             else (attackers bf) - 1
        defendersInOneBattle =
          if defenders bf >= 2
             then 2
             else defenders bf

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

tally :: [DieValue] -> (Army, Army) -> (Army, Army)
tally rands (attackersInOneBattle,_) =
  compareDice ((reverse . sort) attackerDice,(reverse . sort) defenderDice)
              (0,0)
  where attackerDice = take attackersInOneBattle rands
        defenderDice = drop attackersInOneBattle rands
        compareDice ([],_) toDieArmies = toDieArmies
        compareDice (_,[]) toDieArmies = toDieArmies
        compareDice (a:as,d:ds) (toDieAttackers,toDieDefenders) =
          compareDice
            (as,ds)
            (if a > d
                then (toDieAttackers,toDieDefenders + 1)
                else (toDieAttackers + 1,toDieAttackers))


battleOutcome
  :: Battlefield -> (Army,Army) -> Battlefield
battleOutcome bf (toDieAttackers, toDieDefenders) = Battlefield (attackers bf - toDieAttackers) (defenders bf - toDieDefenders)

------------------------------------------------------------
invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | attackers bf < 2 || defenders bf <= 0 = return bf
  | otherwise = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf =
  replicateM 1000
             (invade bf) >>=
  \bfs ->
    return (fromIntegral (length $ filter ((<= 0) . defenders) bfs) /
            fromIntegral (length bfs))
