module Main where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Prelude (String, IO)
import Fana.Prelude

-- ~ import qualified System.Environment as Env
import qualified System.IO as Base
import qualified Technical.Else as Tech
import qualified WriteInTree.CommandLine as ClC
import qualified WriteInTree.Compile as Compile
import qualified WriteInTree.Convert as Convert


handle_error :: ExceptT String IO () -> IO ()
handle_error = runExceptT >=> either (("error: " <>) >>> Base.hPutStrLn Base.stderr) pure

main :: IO ()
main = 
	do
		-- ~ cl_arguments <- Env.getArgs
		ClC.parse_new >>= (program >>> handle_error)

program :: ClC.Command -> ExceptT String IO ()
program command =
	case command of
		ClC.CTranslate ifp ofp -> Compile.compile (Tech.FilePath ofp) ifp
		ClC.CConvert test_idempotence ip op -> Convert.convert test_idempotence ip op
