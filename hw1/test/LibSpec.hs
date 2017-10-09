module LibSpec (main, spec) where

import           Data.List     (sort)
import           Lib
import           System.Random (newStdGen, randomRs)
import           Test.Hspec

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

main :: IO ()
main = do
    hspec spec

spec :: Spec
spec = do
    describe "block1" $ do
        it "order3" $ do
            order3 (5 :: Int, 2, 10) `shouldBe` (2, 5, 10)
            order3 (9 :: Int, 8, 3)  `shouldBe` (3, 8, 9)
        it "highestBit" $ do
            highestBit (15 :: Int) `shouldBe` (8 :: Int)
            highestBit (16 :: Int) `shouldBe` (16 :: Int)
            highestBit (17 :: Int) `shouldBe` (16 :: Int)
        it "smartReplicate" $ do
            smartReplicate [1,2,3] `shouldBe` [1,2,2,3,3,3]
        it "contains" $ do
            contains (3 :: Int) ([[1..5], [2,0], [3,4]] :: [[Int]]) `shouldBe` ([[1..5],[3,4]] :: [[Int]])

    describe "block2" $ do
        it "removeAt" $ do
            removeAt (1 :: Int) ([1,2,3] :: [Int])  `shouldBe` ([1,3] :: [Int])
            removeAt (10 :: Int) ([1,2,3] :: [Int]) `shouldBe` ([1,2,3] :: [Int])
            removeAt (3 :: Int) ([1..5] :: [Int]) `shouldBe` ([1,2,3,5] :: [Int])
            removeAt (2 :: Int) "abc" `shouldBe` "ab"
        it "collectEvery" $ do
            collectEvery 3 ([1..8] :: [Int]) `shouldBe` (([1,2,4,5,7,8], [3,6]) :: ([Int], [Int]))
        it "stringSum" $ do
            stringSum "1 1" `shouldBe` 2
            stringSum "100\n\t-3" `shouldBe` 97
        it "mergeSort" $ do
            mergeSortTest <- randomIntList 5 (-10) 10
            let mergeSortResult = sort mergeSortTest
            mergeSort mergeSortTest `shouldBe` mergeSortResult
    describe "block3" $ do
        it "nextDay" $ do
            nextDay Mon `shouldBe` Tue
            nextDay Tue `shouldBe` Wed
            nextDay Wed `shouldBe` Thu
            nextDay Thu `shouldBe` Fri
            nextDay Fri `shouldBe` Sat
            nextDay Sat `shouldBe` Sun
            nextDay Sun `shouldBe` Mon
        it "afterDays" $ do
            afterDays 2 Mon `shouldBe` Wed
            afterDays 7 Mon `shouldBe` Mon
            afterDays 9 Mon `shouldBe` Wed
        it "isWeekend" $ do
            isWeekend Mon `shouldBe` False
            isWeekend Tue `shouldBe` False
            isWeekend Wed `shouldBe` False
            isWeekend Thu `shouldBe` False
            isWeekend Fri `shouldBe` False
            isWeekend Sat `shouldBe` True
            isWeekend Sun `shouldBe` True
        it "daysToParty" $ do
            daysToParty Mon `shouldBe` 4
            daysToParty Tue `shouldBe` 3
            daysToParty Wed `shouldBe` 2
            daysToParty Thu `shouldBe` 1
            daysToParty Fri `shouldBe` 0
            daysToParty Sat `shouldBe` 6
            daysToParty Sun `shouldBe` 5
        it "fights" $ do
            let m = Monster 5 5
            let k = Knight 5 5
            fight m k `shouldBe` (k, 1, 5)
            let kk1 = Knight 5 6
            let kk2 = Knight 6 5
            fight kk1 kk2 `shouldBe` (kk1, 1, 6)
        it "trees" $ do
            let tree = fromList [1..20] :: Tree Integer
            all (`find` tree) [1..20] `shouldBe` True
            any (`find` tree) [40..90] `shouldBe` False
            count tree `shouldBe` 20
        it "splitOn" $
            splitOn '/' "path/to/file" `shouldBe` ["path", "to", "file"]
    describe "block4" $ do
        it "maybeConcat" $
             maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` ([1..5] :: [Integer])
        it "monoidTree" $ do
            let a = fromList [1..3] :: Tree Integer
            let b = fromList [120,119..78] :: Tree Integer
            let c = fromList [10..63] :: Tree Integer
            (a `mappend` b) `mappend` c `shouldBe` a `mappend` (b `mappend` c)
