module Main where


import Prelude (IO)
import Fana.Prelude

import qualified System.Environment as Env

import qualified Technical.Else as Tech
import qualified WriteInTree.CommandLine as ClC
import qualified WriteInTree.Compile as Compile
import qualified WriteInTree.Document.SepProps.Command_ShowDefault as ShowDefaultProps
import qualified WriteInTree.Convert as Convert


main :: IO ()
main = 
	do
		cl_arguments <- Env.getArgs
		ClC.parse_new >>= program

program :: ClC.Command -> IO ()
program command =
	case command of
		ClC.CTranslate ifp ofp sentencing -> Compile.compile sentencing (Tech.FilePath ofp) ifp
		ClC.CShowDefaultDocProps -> ShowDefaultProps.doit
		ClC.CConvert test_idempotence ip op -> Convert.convert test_idempotence ip op
