module WriteInTree.ChangeIdsToMachine
(
	change_ids_to_machine,
)
where

import Prelude (IO)
import Fana.Prelude
import Technical.Else (tell_error)
import System.FilePath (FilePath)

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Data as D
import qualified WriteInTree.Document.Core.Document as DocData
import qualified WriteInTree.Document.Core.Serial.Page.Tree as Page
import qualified WriteInTree.Document.Data as Data
import qualified WriteInTree.Document.File as File


type Text = Base.String

type Doc = File.DocData
type DocCore = File.DocCoreData

change_ids_to_machine :: FilePath {- input -} -> FilePath {- output -} -> IO ()
change_ids_to_machine input_address output_address =
	let
		change_doc :: Doc -> Doc
		change_doc doc =
			Optic.fn_up
				Data.not_sub_page_address_in_Document
				(Page.node_idu_to_machine (DocData.docSite (D.doc_core doc)))
				doc
		in
			(map >>> map) change_doc (File.read'' input_address)
			>>= Base.either tell_error (File.write'' output_address)
