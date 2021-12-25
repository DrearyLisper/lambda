module Main where

import Test.Hspec

import Lambda.Parsing
import Lambda.Types

main :: IO ()
main = hspec $ do
  describe "Lambda.Parsing.parseExpression" $ do
    it "can parse names" $ do
      parseExpression "x" `shouldBe` (Name "x", "")
