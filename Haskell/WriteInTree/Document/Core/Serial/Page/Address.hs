module WriteInTree.Document.Core.Serial.Page.Address
(
	ParseError, layer,
)
where

import Fana.Math.Algebra.Monoid.Accumulate (Accumulated)
import Fana.Prelude
import Prelude (String)
import WriteInTree.Document.Core.Data (nodePosition, nodeLabels)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))

import qualified Data.Bifunctor as BiFr
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.Page.BreakStructure as BS
import qualified WriteInTree.Document.Core.Serial.Page.Count as Count
import qualified WriteInTree.Document.Core.Serial.Page.Data as PData
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos

type Text = String
type PageKey = Count.Ordinal


validate_address :: Maybe PageAddress -> Either (Accumulated Text) PageAddress
validate_address = maybe (Left (Acc.single "page has no address")) Right

type ParseError = Pos.PositionedMb (Accumulated Text)
type ParseFailable = Either ParseError

get_address_from_Node :: Data.Node i -> ParseFailable PageAddress
get_address_from_Node node =
	BiFr.first (Pos.position_error_mb (nodePosition node))
		(validate_address ((nodeLabels >>> Label.address_of_Labels) node))

get_address_of_page :: BS.Page a (Data.Node i) -> ParseFailable PageAddress
get_address_of_page = BS.pageContent >>> BS.pcTrunk >>> get_address_from_Node

finalize_page :: BS.Page a (Data.Node i) -> ParseFailable (PageAddress, BS.Page a (Data.Node i))
finalize_page page = map (\ a -> (a, page)) (get_address_of_page page)

layer ::
	Optic.PartialIso ParseError
		(Data.StructureAsTree i1)
		(PData.SiteStructure (BS.Page a (Data.Node i2)))
		(Data.StructureAsTree i1)
		(PData.SiteStructure (PageAddress, BS.Page a (Data.Node i2)))
layer = Optic.PartialIso id (traverse finalize_page)
		
