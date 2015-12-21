module RPGSpec (main, spec) where

import Test.Hspec
import RPG
import Control.Monad (replicateM)
import Control.Monad.Trans.State

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let me = Stats 8 5 5
  let boss = Stats 12 7 2
  let game = Game me boss Player
  describe "equipPlayer" $ do
    let weap = Weapon (Item "Dagger" 8 4 0)
    let baseStats = Stats 100 4 0
    it "should turn a weapon into added damage" $ do
      let equip = Equipment weap Nothing Nothing Nothing
      equipPlayer equip `shouldBe` baseStats
    it "should turn armor into added armor" $ do
      let armor' = Armor (Item "Leather" 13 0 1)
          equip = Equipment weap (Just armor') Nothing Nothing
      equipPlayer equip `shouldBe` baseStats { armor = 1 }
  describe "fight" $ do
    let game1 = game { enemyStats = boss { hp = 9 }, turn = Enemy }
        game2 = game1 { playerStats = me { hp = 6 }, turn = Player }
        game3 = game2 { enemyStats = boss { hp = 6 }, turn = Enemy }
        game4 = game3 { playerStats = me { hp = 4 }, turn = Player }
        game5 = game4 { enemyStats = boss { hp = 3 }, turn = Enemy }
        game6 = game5 { playerStats = me { hp = 2 }, turn = Player }
        game7 = game6 { enemyStats = boss { hp = 0 }, turn = Enemy }
    it "should run one round of a fight" $
      execState fight game `shouldBe` game1
    it "should run several rounds as expected" $ do
         execState (replicateM 2 fight) game `shouldBe` game2
         execState (replicateM 3 fight) game `shouldBe` game3
         execState (replicateM 4 fight) game `shouldBe` game4
         execState (replicateM 5 fight) game `shouldBe` game5
         execState (replicateM 6 fight) game `shouldBe` game6
         execState (replicateM 7 fight) game `shouldBe` game7
  describe "playerWins" $
    it "should return True if the player won fight" $
      playerWins me boss `shouldBe` True
