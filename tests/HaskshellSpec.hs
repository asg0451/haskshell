{-# LANGUAGE LambdaCase #-}

module HaskshellSpec (main, spec) where

import           Control.Monad.State.Lazy
import           Data.Maybe
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process
import           Test.Hspec

import           Haskshell                as H
import           Lexer                    as L
import           Parser                   as P
import           Types                    as T

main :: IO ()
main = hspec spec

runHaskshell :: String -> IO (Val, InternalState)
runHaskshell s = runStateT (eval ast) initialState
    where ast = plex s

runHaskshellState :: String -> InternalState -> IO (Val, InternalState)
runHaskshellState s state = runStateT (eval ast) state
    where ast = plex s


spec :: Spec
spec = do
  describe "builtins" $ do
         describe "regular cd functionality" $ do
                          it "goes to home when no args" $ do
                                         home <- getEnv "HOME"
                                         setCurrentDirectory "/tmp"

                                         runHaskshell "cd"
                                         dir' <- getCurrentDirectory
                                         dir' `shouldBe` home
                          it "goes to given dir" $ do
                                                 home <- getEnv "HOME"
                                                 setCurrentDirectory "/tmp"

                                                 runHaskshell "cd /etc"
                                                 dir' <- getCurrentDirectory
                                                 dir' `shouldBe` "/etc"

                          it "cd ..'s correctly" $ do
                                                 setCurrentDirectory "/tmp"

                                                 runHaskshell "cd .."
                                                 dir <- getCurrentDirectory
                                                 dir `shouldBe` "/"

         describe "pushd, popd" $ do
                          it "can pushd" $ do
                                    (res, _) <- runHaskshell "pushd ."
                                    res `shouldBe` ExitSuccess

                          it "can popd" $ do
                                    (res, _) <- runHaskshell "popd"
                                    res `shouldBe` ExitSuccess

                          it "pushd/popd's correctly once" $ do
                                    home <- getEnv "HOME"
                                    setCurrentDirectory "/tmp"

                                    runHaskshell "pushd ."
                                    setCurrentDirectory home
                                    runHaskshell "popd"
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
