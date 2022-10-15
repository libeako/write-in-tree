module WriteInTree.FillPageAddresses
(
	-- ~ fill_page_addresses,
)
where

import Prelude (IO)
import Fana.Prelude
-- ~ import Technical.Else (tell_error)
import System.FilePath (FilePath)

import qualified Prelude as Base
import qualified WriteInTree.Document.File as File


type Text = Base.String

type Doc = File.DocData''
type DocCore = File.DocCoreData''

write :: Bool -> FilePath -> Doc -> IO ()
write do_readback_test address doc = 
	do
		File.write'' address doc



-- ~ fill_page_addresses :: FilePath {- the addresses to fill with -} -> FilePath {- input -} -> FilePath {- output -} -> IO ()
-- ~ fill_page_addresses addresses_file input_address output_address = 
	-- ~ let
		-- ~ from_doc_result :: Either File.Error Doc -> IO ()
		-- ~ from_doc_result = Base.either tell_error (write do_readback_test output_address)
		-- ~ in File.read'' input_address >>= from_doc_result
