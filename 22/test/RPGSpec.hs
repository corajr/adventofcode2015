module RPGSpec (main, spec) where

import Test.Hspec
import RPG
import Control.Monad (replicateM)
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let me = mtPlayer { hp = 10, mana = 250 }
      boss1 = BossStats 13 8
      spells1 = [ Later Poison
                , Now MagicMissile
                ]
      game1 = mtGame { playerStats = PS me
                     , enemyStats = BS boss1
                     , spellList = spells1 }
      spells2 = [ Later Recharge
                  , Later Shield
                  , Now Drain
                  , Later Poison
                  , Now MagicMissile
                  ]
      boss2 = BossStats 14 8
      poison = getEffect Poison
      recharge = getEffect Recharge
      shield = getEffect Shield
      poisonE = Map.singleton Poison poison
  describe "oneRound" $ do
    let game2 = game1 { playerStats = PS me { mana = 77 }
                      , enemyStats = BS boss1
                      , activeEffects = poisonE
                      , spellList = [Now MagicMissile]
                      , turn = E Enemy }
    let game3 = game1 { playerStats = PS me { hp = 2, mana = 77 }
                      , enemyStats = BS $ boss1 { hp' = 10 }
                      , activeEffects = Map.singleton Poison (poison { eDuration = 5})
                      , spellList = [Now MagicMissile]
                      , turn = P Player }
    let game4 = game1 { playerStats = PS me { hp = 2, mana = 24 }
                      , enemyStats = BS $ boss1 { hp' = 3 }
                      , activeEffects = Map.singleton Poison (poison { eDuration = 4})
                      , spellList = []
                      , turn = E Enemy}
    it "should run one round of a fight" $
      execGame oneRound game1 `shouldBe` Right game2
    it "should run several rounds as expected" $ do
      execGame (replicateM 1 oneRound) game1 `shouldBe` Right game2
      execGame (replicateM 2 oneRound) game1 `shouldBe` Right game3
      execGame (replicateM 3 oneRound) game1 `shouldBe` Right game4
    let game1' = game1 { enemyStats = BS boss2
                       , spellList = spells2 }
    let game2' = game1' { playerStats = PS me { mana = 21 }
                        , activeEffects = Map.singleton Recharge (recharge { eDuration = 5 })
                        , spellList = tail spells2
                        , turn = E Enemy
                        }
    let game3' = game1' { playerStats = PS me { hp = 2, mana = 122 }
                        , activeEffects = Map.singleton Recharge (recharge { eDuration = 4 })
                        , spellList = tail spells2
                        , turn = P Player
                        }
    let game4' = game1' { playerStats = PS me { hp = 2, armor = 7, mana = 110 }
                        , activeEffects = Map.fromList [ (Recharge, recharge { eDuration = 3 })
                                                       , (Shield, shield { eDuration = 5 })
                                                       ]
                        , spellList = drop 2 spells2
                        , turn = E Enemy
                        }
    let game5' = game1' { playerStats = PS me { hp = 1, armor = 7, mana = 211 }
                        , activeEffects = Map.fromList [ (Recharge, recharge { eDuration = 2 })
                                                       , (Shield, shield { eDuration = 4 })
                                                       ]
                        , spellList = drop 2 spells2
                        , turn = P Player
                        }
    let game6' = game1' { playerStats = PS me { hp = 3, armor = 7, mana = 239 }
                        , enemyStats = BS boss2 { hp' = 12}
                        , activeEffects = Map.fromList [ (Recharge, recharge { eDuration = 1 })
                                                       , (Shield, shield { eDuration = 3 })
                                                       ]
                        , spellList = drop 3 spells2
                        , turn = E Enemy
                        }
    let game7' = game1' { playerStats = PS me { hp = 2, armor = 7, mana = 340 }
                        , enemyStats = BS boss2 { hp' = 12}
                        , activeEffects = Map.fromList [ (Shield, shield { eDuration = 2 })]
                        , spellList = drop 3 spells2
                        , turn = P Player
                        }
    let game8' = game1' { playerStats = PS me { hp = 2, armor = 7, mana = 167 }
                        , enemyStats = BS boss2 { hp' = 12}
                        , activeEffects = Map.fromList [ (Shield, shield { eDuration = 1 })
                                                       , (Poison, poison { eDuration = 6})]
                        , spellList = drop 4 spells2
                        , turn = E Enemy
                        }
    let game9' = game1' { playerStats = PS me { hp = 1, armor = 7, mana = 167 }
                        , enemyStats = BS boss2 { hp' = 9 }
                        , activeEffects = Map.fromList [ (Poison, poison { eDuration = 5}) ]
                        , spellList = drop 4 spells2
                        , turn = P Player
                        }
    let game10' = game1' { playerStats = PS me { hp = 1, armor = 0, mana = 114 }
                         , enemyStats = BS boss2 { hp' = 2 }
                         , activeEffects = Map.fromList [ (Poison, poison { eDuration = 4}) ]
                         , spellList = []
                         , turn = E Enemy
                         }
    it "should run several rounds of a different  game" $ do
      execGame (replicateM 1 oneRound) game1' `shouldBe` Right game2'
      execGame (replicateM 2 oneRound) game1' `shouldBe` Right game3'
      execGame (replicateM 3 oneRound) game1' `shouldBe` Right game4'
      execGame (replicateM 4 oneRound) game1' `shouldBe` Right game5'
      execGame (replicateM 5 oneRound) game1' `shouldBe` Right game6'
      execGame (replicateM 6 oneRound) game1' `shouldBe` Right game7'
      execGame (replicateM 7 oneRound) game1' `shouldBe` Right game8'
      execGame (replicateM 8 oneRound) game1' `shouldBe` Right game9'
      execGame (replicateM 9 oneRound) game1' `shouldBe` Right game10'
    it "should throw an exception if there are not enough spells" $
      execGame (replicateM 5 oneRound) game1 `shouldSatisfy` isLeft
    it "should throw an exception if a spell is repeated too soon" $
      execGame (replicateM 3 oneRound) game1 {spellList = [Later Poison, Later Poison]} `shouldSatisfy` isLeft
  describe "playerWins" $
    it "should return True if the player won fight" $ do
      playerWins (BS boss1) spells1 `shouldBe` True
      playerWins (BS boss2) spells2 `shouldBe` True
