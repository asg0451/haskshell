module HaskshellSpec (main, spec) where

import           Control.Monad.State.Lazy
import           Data.Maybe
import           Haskshell                as H
import           Lexer                    as L
import           Parser                   as P
import           System.Directory
import           System.Environment
import           System.IO
import           System.Process
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "builtins" $ do
         describe "cd functionality" $ do
                          it "goes to home when no args" $ do
                                         home <- getEnv "HOME"
                                         setCurrentDirectory "/tmp"
                                         runStateT (eval ast) initialState
                                         dir' <- getCurrentDirectory
                                         dir' `shouldBe` home
                                             where ast = plex "cd"
