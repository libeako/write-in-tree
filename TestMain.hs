module Main where

import qualified System.IO as Sys

import Fana.Develop.Test.Define
import Fana.Prelude

import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Develop.Test.Run as Test

import qualified WriteInTree.Document.Core.Serial.Id.Node as IdN
import qualified WriteInTree.Document.Core.Serial.Id.Forest as IdF


fail :: Test
fail = Test.single "dummy test" False

all_tests :: Test
all_tests = Test.bunch "all" 
	[ IdN.test, IdF.test
	]

main :: Sys.IO ()
main = Test.run all_tests
