module WriteInTree.FillPageAddresses
(
	fill_page_addresses,
)
where

import Prelude (IO)
import Control.Monad.State.Lazy (State)
import Fana.Prelude
import Technical.Else (tell_error)
import System.FilePath (FilePath)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))

import qualified Control.Monad.State.Lazy as State
import qualified Data.List as List
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.File as File
import qualified WriteInTree.Document.Main as Data


type Text = Base.String

type Doc = File.DocData
type DocCore = File.DocCoreData

write :: FilePath -> Doc -> IO ()
write address doc = File.write'' address doc

algorithm' :: [Text] -> Doc -> Doc
algorithm' new_addresses =
	let
		step :: Maybe PageAddress -> State [Text] (Maybe PageAddress)
		step _ =
			do
				addresses <- State.get
				let current_address = List.head addresses
				State.modify List.tail
				pure (Just (PageAddress current_address))
		in Optic.traverse Data.page_addresses_in_doc step >>> flip State.evalState new_addresses

algorithm :: Text -> Doc -> Doc
algorithm = List.lines >>> algorithm'

fill_page_addresses :: FilePath {- the addresses to fill with -} -> FilePath {- input -} -> FilePath {- output -} -> IO ()
fill_page_addresses addresses_file input_address output_address = 
	do
		addresses_string <- Base.readFile addresses_file
		input_doc_result <- File.read'' input_address
		Base.either tell_error (algorithm addresses_string >>> write output_address) input_doc_result
