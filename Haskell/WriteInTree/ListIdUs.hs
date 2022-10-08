module WriteInTree.ListIdUs
(
	list_idus,
)
where

import Control.Monad ((>=>))
import Fana.Prelude
import Prelude (fmap, String, (++), IO, FilePath)

import qualified Data.Bifunctor as BiFu
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Data.Either as Base
import qualified Fana.Data.HeteroPair as HePair
import qualified Fana.Data.Identified as Identified
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified WriteInTree.Document.Data as DocData
import qualified WriteInTree.Document.File as DocRead
import qualified WriteInTree.Output.Technical as Ot


type Text = Base.String


identified_node_text_frame :: (String, [String]) -> String
identified_node_text_frame (identifier, path_to_trunk) = 
	"\n" ++ identifier ++ " at " ++ (List.intercalate " -> " path_to_trunk)

identified_node_text :: (Data.NodeIdU, Label.Elem Data.NodeIdU ()) -> String
identified_node_text = 
	BiFu.bimap (Identified.cargo >>> Data.nidun_u) Pos.get_position
	>>> identified_node_text_frame

identified_nodes_of_doc :: Data.Document id_u ia -> [(id_u, Data.Node id_u ia)]
identified_nodes_of_doc = 
	Data.docTree >>> Optic.to_list Data.node_in_tree >>> map Data.attach_its_uid_to_node >>> Base.catMaybes

list_idus :: FilePath {- input folder path -} -> IO ()
list_idus = 
	let
		error_to_output :: DocRead.Error -> Ot.Output
		error_to_output = Fana.show >>> Accu.extract >>> Left
		doc_to_output :: DocData.Data Data.NodeIdU Data.NodeIdU -> Ot.Output
		doc_to_output = 
			DocData.doc_core
			>>> identified_nodes_of_doc 
			>>> fmap (BiFu.second Data.nodeWitSource >>> identified_node_text)
			>>> Foldable.concat >>> HePair.before Ot.empty_fileops >>> Right
		in DocRead.read >=> (Base.either error_to_output doc_to_output >>> Ot.write)
