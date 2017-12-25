{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec

import           Lens       ((%~), (.~), (^.), _1, _2)

main :: IO ()
main = hspec $ do
  describe "(.~) test" $ do
    it "((2 :: Integer), (4 :: Integer))._1 = (5 :: Integer)" $
        (_1 .~ (5 :: Integer) $ (2 :: Integer, 4 :: Integer)) `shouldBe` (5 :: Integer, 4 :: Integer)
    it "((2 :: Integer), (4 :: Integer))._2 = (5 :: Integer)" $
        (_2 .~ (5 :: Integer) $ (2 :: Integer, 5 :: Integer)) `shouldBe` (2 :: Integer, 5 :: Integer)
  describe "(%~) test" $ do
    it "((2 :: Integer), (4 :: Integer))._1 += (2 :: Integer)" $
        (_1 %~ (+(2 :: Integer)) $ (2 :: Integer, 4 :: Integer)) `shouldBe` (4 :: Integer, 4 :: Integer)
    it "((2 :: Integer), (4 :: Integer))._2 -= (5 :: Integer)" $
        (_2 %~ (+(5 :: Integer)) $ (2 :: Integer, 4 :: Integer)) `shouldBe` (2 :: Integer, 9 :: Integer)
  describe "(^.) test" $ do
    it "(((2 :: Integer), (4 :: Integer))._1 == (2 :: Integer)" $
        (_1 ^. (2 :: Integer, 4 :: Integer)) `shouldBe` (2 :: Integer)
    it "((2 :: Integer), (4 :: Integer))._2 = (4 :: Integer)" $
        (_2 ^. (2 :: Integer, 4 :: Integer)) `shouldBe` (4 :: Integer)
