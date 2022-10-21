module Main where


import Prelude (IO)
import Fana.Prelude

import qualified System.Environment as Env

import qualified Technical.Else as Tech
import qualified WriteInTree.ChangeIdsToMachine as ChangeIdsToMachine
import qualified WriteInTree.CommandLine as ClC
import qualified WriteInTree.Compile as Compile
import qualified WriteInTree.Convert as Convert
import qualified WriteInTree.Document.SepProps.Command_ShowDefault as ShowDefaultProps
import qualified WriteInTree.FillPageAddresses as PageAddresses


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
		ClC.CFillPageAddresses new_addresses ip op -> PageAddresses.fill_page_addresses new_addresses ip op
		ClC.CChangeIdsToMachine ip op -> ChangeIdsToMachine.change_ids_to_machine ip op
