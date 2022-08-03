module WriteInTree.Document.Core.Serial.Parse
(
	parse_from_string,
)
where

import Fana.Prelude
import Prelude (String)

import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.Layers as CoreSerial
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified WriteInTree.Document.SepProps.Data as SepProps


type Text = String


delete_additional_info_from_node :: Data.Node a id_u ia e -> Data.Node () id_u ia e
delete_additional_info_from_node = id
	>>> Optic.fill Data.ofNode_additional ()

parse_from_string ::
	SepProps.DocSepProps ->
	String {- input file content -} -> 
	Either (Pos.PositionedMb (Accu.Accumulated Text)) (Data.Document () Data.NodeIdU Data.NodeIdU Text)
parse_from_string sep_props = 
	(Optic.piso_interpret (CoreSerial.layer sep_props))
	>>> map (Optic.fn_up Data.tree_in_Document (map delete_additional_info_from_node))
