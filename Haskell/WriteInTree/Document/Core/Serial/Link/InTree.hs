module WriteInTree.Document.Core.Serial.Link.InTree
(
	serialize,
)
where

import Data.Tree (Tree (..), Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Data (Inline, Link)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (..))

import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.Link.Individual as Individual
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String

type ParseError = Pos.Positioned (Accu.Accumulated Text)

render :: Tree (Positioned Inline) -> Tree Text
render (Node trunk children) =
	let
		trunk_rendered :: Text
		trunk_rendered = Data.ilVisual (positionedValue trunk)
		children_rendered :: [Tree Text]
		children_rendered = map render children
		in
			case (positionedValue >>> Data.ilLink) trunk of
				Nothing -> Node trunk_rendered children_rendered
				Just link -> Node trunk_rendered (Individual.render link : children_rendered)

type ParseChildrenSituation =
	(Maybe Link, Forest (Positioned Text) {- <- the rest of the children -})

parse_children :: Forest (Positioned Text) -> Either ParseError ParseChildrenSituation
parse_children children =
	case children of
		[] -> Right (Nothing, [])
		first : rest ->
			maybe
				(Right (Nothing, children))
				(map (\ l -> (Just l, rest)))
				(Individual.parse first)

parse :: Tree (Positioned Text) -> Either ParseError (Tree (Positioned Inline))
parse (Node trunk children) =
	let
		from_situation :: ParseChildrenSituation -> Either ParseError (Tree (Positioned Inline))
		from_situation (l, rest_of_children) =
			map (Node (map (flip Data.Inline l) trunk)) (traverse parse rest_of_children)
		in parse_children children >>= from_situation

serialize :: 
	Optic.PartialIso ParseError 
		(Forest Text) (Forest (Positioned Text))
		(Forest (Positioned Inline)) (Forest (Positioned Inline))
serialize = Optic.lift_piso (Optic.PartialIso render parse)
