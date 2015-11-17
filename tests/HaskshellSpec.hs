module HaskshellSpec (main, spec) where

import qualified Haskshell      as H
import           System.IO
import           System.Process
import           Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "blank" $ do
         it "does something" $ do
                       (in_,out, _, _) <- createProcess (shell "/home/miles/.local/bin/Haskshell")
                       case in_ of
                         Nothing -> print "wat"
                         Just i -> do
                                  hSetBuffering i LineBuffering
                                  hGetLine i >>= print
                                  1 `shouldBe` 1
