module VarsSpec (main, spec) where

import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process

import           System.IO.Silently
import           Test.Hspec
import           Test.Hspec.Core.Hooks
import           Test.Hspec.Expectations

import           Haskshell               as H
import           Lexer                   as L
import           Parser                  as P
import           TestUtils
import           Types                   as T

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "vars" $ do
         describe "setting/unsetting" $ do
                      it "can set vars" $ do
                                          -- assume "x" isnt a set var in the initial state
                                          (res, st) <- runHaskshell "x = 42"
                                          res `shouldBe` ExitSuccess
                                          (output, (res', st')) <- capture $ runHaskshellState "echo -n $x" st
                                          res' `shouldBe` ExitSuccess
                                          output `shouldBe` "42"
