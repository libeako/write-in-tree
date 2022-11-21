module WriteInTree.Document.Core.Serial.Page.Serialize
(
	ParseError, layer,
)
where

import Data.Tree (Tree, Forest)
import Fana.Math.Algebra.Monoid.Accumulate (Accumulated)
import Fana.Prelude
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Page.Data (Page, PageContent)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure 
	(PageAddress (..), address_of_Labels)
import WriteInTree.Document.Core.Serial.RichTextTree.Position 
	(PositionedMb, position_error, maybefy_positioned)

import qualified Data.Tree as Tree
import qualified Fana.Data.Key.Map.KeyIsString as StringyMap
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


type PageMap = StringyMap.Map Base.Char PageContent

type ParseError = PositionedMb (Accumulated Text)
type ParseFailable = Either ParseError

get_address_of_node :: Node -> ParseFailable PageAddress
get_address_of_node node =
	case address_of_Labels (nodeLabels node) of
		Nothing ->
			let 
				error = Acc.single "page trunk node does not have address."
				positioned_error = maybefy_positioned (position_error node error)
				in Left positioned_error
		Just address -> Right address

parse_page :: Tree Node -> ParseFailable Page
parse_page (Tree.Node trunk children) =
	let
		from_address :: PageAddress -> Page
		from_address a = (a, (trunk, children))
		in map from_address (get_address_of_node trunk)

layer :: Optic.PartialIso ParseError (Forest Node) (Tree Node) Page Page
layer = Optic.PartialIso (snd >>> snd) parse_page
