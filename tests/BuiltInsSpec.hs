{-# LANGUAGE LambdaCase #-}

module BuiltInsSpec (main, spec) where

import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process

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
  describe "builtins" $ do
         describe "regular cd functionality" $ do
                          home <- runIO $ getEnv "HOME"

                          it "goes to home when no args" $ do
                                         setCurrentDirectory "/tmp"

                                         runHaskshell "cd"
                                         dir' <- getCurrentDirectory
                                         dir' `shouldBe` home
                          it "goes to given dir" $ do
                                                 setCurrentDirectory "/tmp"
                                                 runHaskshell "cd /etc"
                                                 dir' <- getCurrentDirectory
                                                 dir' `shouldBe` "/etc"

                          it "cd ..'s correctly" $ do
                                                 setCurrentDirectory "/tmp"
                                                 runHaskshell "cd .."
                                                 dir <- getCurrentDirectory
                                                 dir `shouldBe` "/"

                          it "cd -'s correctly" $ do
                                                 setCurrentDirectory "/tmp"
                                                 setCurrentDirectory home
                                                 (res, _) <- runHaskshell "cd -"
                                                 res `shouldBe` ExitSuccess
                                                 dir <- getCurrentDirectory
                                                 dir `shouldBe` "/tmp"


         describe "pushd, popd functionality" $ do
                          it "can pushd" $ do
                                    (res, _) <- runHaskshell "pushd ."
                                    res `shouldBe` ExitSuccess

                          it "can popd" $ do
                                    (res, _) <- runHaskshell "popd"
                                    res `shouldBe` ExitSuccess

                          it "pushd/popd's correctly once" $ do
                                    home <- getEnv "HOME"
                                    setCurrentDirectory "/tmp"

                                    (res, _) <- runHaskshell "pushd ."
                                    res `shouldBe` ExitSuccess
                                    setCurrentDirectory home
                                    (res', _) <- runHaskshell "popd"
                                    res' `shouldBe` ExitSuccess
                                    dir <- getCurrentDirectory
                                    dir `shouldBe` "/tmp"

         describe "alias" $ do
                          it "can alias" $ do
                              -- assume echo works
                              (res1, state) <- runHaskshell "alias wat = echo"
                              res1 `shouldBe` ExitSuccess
                              (res2, _) <- runHaskshellState "wat \"hi\"" state
                              res2 `shouldBe` ExitSuccess

                          it "can unalias" $ do
                              -- assume wat is not a command
                              (res1, state1) <- runHaskshell "alias wat = echo"
                              res1 `shouldBe` ExitSuccess
                              (res2, state2) <- runHaskshellState "wat \"hi\"" state1
                              res2 `shouldBe` ExitSuccess
                              (res3, state3) <- runHaskshellState "unalias wat" state2
                              res3 `shouldBe` ExitSuccess
                              (res4, state4) <- runHaskshellState "wat \"hi\"" state3
                              res4 `shouldSatisfy` \case
                                   ExitFailure _ -> True
                                   otherwise -> False
