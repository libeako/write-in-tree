module WriteInTree.Document.Core.Serial.Link.InTree
(
	layer,
)
where

import Data.Tree (Tree (..), Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Data (Inline, Link)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (..))
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (Labels)

import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.Link.Individual as Individual
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String
type LabeledPositioned e = (Labels, Positioned e)

type ParseError = Pos.Positioned (Accu.Accumulated Text)

render :: Tree (LabeledPositioned Inline) -> Tree (LabeledPositioned Text)
render (Node trunk children) =
	let
		trunk_rendered :: LabeledPositioned Text
		trunk_rendered = (map >>> map) Data.ilVisual trunk
		children_rendered :: [Tree (LabeledPositioned Text)]
		children_rendered = map render children
		in
			case (snd >>> positionedValue >>> Data.ilLink) trunk of
				Nothing -> Node trunk_rendered children_rendered
				Just link -> Node trunk_rendered (Individual.render' link : children_rendered)

type ParseChildrenSituation =
	(Maybe Link, Forest (LabeledPositioned Text) {- <- the rest of the children -})

parse_children :: Forest (LabeledPositioned Text) -> Either ParseError ParseChildrenSituation
parse_children children =
	case children of
		[] -> Right (Nothing, [])
		first : rest ->
			maybe
				(Right (Nothing, children))
				(map (\ l -> (Just l, rest)))
				(Individual.parse' first)

parse :: Tree (LabeledPositioned Text) -> Either ParseError (Tree (LabeledPositioned Inline))
parse (Node trunk children) =
	let
		from_situation :: ParseChildrenSituation -> Either ParseError (Tree (LabeledPositioned Inline))
		from_situation (l, rest_of_children) =
			map (Node ((map >>> map) (flip Data.Inline l) trunk)) (traverse parse rest_of_children)
		in parse_children children >>= from_situation

layer :: 
	Optic.PartialIso ParseError 
		(Forest (LabeledPositioned Text)) (Forest (LabeledPositioned Text))
		(Forest (LabeledPositioned Inline)) (Forest (LabeledPositioned Inline))
layer = Optic.lift_piso (Optic.PartialIso render parse)
