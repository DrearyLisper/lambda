module Main where

import Test.Hspec

import Control.Monad
import qualified Data.Map as Map

import Lambda.Parsing
import Lambda.Types
import Lambda.Evaluation


-- (\x.(x x) (\f.(f (f y)) \x.x))

testForms :: [(String, String)]
testForms = [
  ("x", "x"),
  ("\\x.x", "\\x.x"),
  ("\\x.(x x)", "\\x.(x x)"),
  ("(\\x.x s)", "s"),
  ("(\\x.(x x) (\\f.(f (f y)) \\x.x))", "(y y)"),
  ("(\\s.(s s) \\x.x)", "\\x.x"),

  ("(\\true.(\\false.true \\x.\\y.y) \\x.\\y.x)", "\\x.\\y.x"),
  ("(\\true.(\\false.false \\x.\\y.y) \\x.\\y.x)", "\\x.\\y.y"),

  ("(\\true.(\\false.(\\or.((or false) false) \\p.\\q.((p p) q)) \\x.\\y.y) \\x.\\y.x)", "\\x.\\y.y"),
  ("(\\true.(\\false.(\\or.((or true) false) \\p.\\q.((p p) q)) \\x.\\y.y) \\x.\\y.x)", "\\x.\\y.x"),
  ("(\\true.(\\false.(\\or.((or false) true) \\p.\\q.((p p) q)) \\x.\\y.y) \\x.\\y.x)", "\\x.\\y.x"),
  ("(\\true.(\\false.(\\or.((or true) true) \\p.\\q.((p p) q)) \\x.\\y.y) \\x.\\y.x)", "\\x.\\y.x"),

  ("(\\true.(\\false.(\\or.((or false) false) \\p.\\q.((p q) p)) \\x.\\y.y) \\x.\\y.x)", "\\x.\\y.y"),
  ("(\\true.(\\false.(\\or.((or true) false) \\p.\\q.((p q) p)) \\x.\\y.y) \\x.\\y.x)", "\\x.\\y.y"),
  ("(\\true.(\\false.(\\or.((or false) true) \\p.\\q.((p q) p)) \\x.\\y.y) \\x.\\y.x)", "\\x.\\y.y"),
  ("(\\true.(\\false.(\\or.((or true) true) \\p.\\q.((p q) p)) \\x.\\y.y) \\x.\\y.x)", "\\x.\\y.x"),

  ("(\\zero.(\\succ.zero \\n.\\f.\\x.(f ((n f) x))) \\f.\\x.x)", "\\f.\\x.x"),
  ("(\\zero.(\\succ.(succ zero) \\n.\\f.\\x.(f ((n f) x))) \\f.\\x.x)", "\\f.\\x.(f x)"),
  ("(\\zero.(\\succ.(succ (succ zero)) \\n.\\f.\\x.(f ((n f) x))) \\f.\\x.x)", "\\f.\\x.(f (f x))"),
  ("(\\zero.(\\succ.(succ (succ (succ zero))) \\n.\\f.\\x.(f ((n f) x))) \\f.\\x.x)", "\\f.\\x.(f (f (f x)))"),
  ("(\\h.\\f.(h f) f)", "\\r-f.(f r-f)")
  ]

main :: IO ()
main = hspec $ do
  describe "Lambda.Parsing.parseExpression" $ do
    it "can parse names" $ do
      parseExpression "x" `shouldBe` (Name "x", "")
      parseExpression "1" `shouldBe` (Name "1", "")
    it "can parse applications" $ do
      parseExpression "(x x)" `shouldBe` (Application (Name "x") (Name "x"), "")
      parseExpression "(\\x.x x)" `shouldBe` ((Application
                                              (Function
                                               (Name "x")
                                               (Name "x"))
                                              (Name "x")), "")
    it "can parse functions" $ do
      parseExpression "\\x.x" `shouldBe` (Function (Name "x") (Name "x"), "")

  describe "Lambda.Parsing.formatExpression" $ do
    it "can parse and format" $ do
      forM_ (map fst testForms) $ \x -> x `shouldBe` formatExpression (fst $ parseExpression x)

  describe "Lambda.Evaluation.eval" $ do
    it "can eval" $ do
      forM_ testForms $ \(a, b) -> b `shouldBe` formatExpression (snd $ eval (Map.empty,  fst $ parseExpression a))
