module Main where

import Test.Hspec

import Lambda.Parsing
import Lambda.Types


-- (\x.(x x) (\f.(f (f y)) \x.x))

main :: IO ()
main = hspec $ do
  describe "Lambda.Parsing.parseExpression" $ do
    it "can parse names" $ do
      parseExpression "x" `shouldBe` (Name "x", "")
      parseExpression "1" `shouldBe` (Name "1", "")
    it "can parse applications" $ do
      parseExpression "(x x)" `shouldBe` ((Application (Name "x") (Name "x")), "")
      parseExpression "(\\x.x x)" `shouldBe` ((Application
                                              (Function
                                               (Name "x")
                                               (Name "x"))
                                              (Name "x")), "")
    it "can parse functions" $ do
      parseExpression "\\x.x" `shouldBe` ((Function (Name "x") (Name "x")), "")
