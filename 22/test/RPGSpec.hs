module RPGSpec (main, spec) where

import Test.Hspec
import RPG
import Control.Monad (replicateM)
import Control.Monad.Trans.State
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
      poison = getEffect Poison
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
      execState oneRound game1 `shouldBe` game2
    it "should run several rounds as expected" $ do
         execState (replicateM 1 oneRound) game1 `shouldBe` game2
         execState (replicateM 2 oneRound) game1 `shouldBe` game3
         execState (replicateM 3 oneRound) game1 `shouldBe` game4
  describe "playerWins" $
    it "should return True if the player won fight" $ do
      playerWins (BS boss1) spells1 `shouldBe` True
