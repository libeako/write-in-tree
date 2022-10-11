module WriteInTree.Document.Core.Serial.Link.InTree
(
	layer,
)
where

import Data.Tree (Tree (..), Forest)
import Fana.Prelude

import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.Serial.Link.Individual as Individual
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String
type A = Label.Elem Text -- additional info wrapper
type Inline = Data.Inline Text

type ParseError = Pos.Positioned (Accu.Accumulated Text)

render :: Tree (A Inline) -> Tree (A Text)
render (Node trunk children) =
	let
		trunk_rendered :: A Text
		trunk_rendered = map Data.ilVisual trunk
		children_rendered :: [Tree (A Text)]
		children_rendered = map render children
		in
			case Data.ilLink (Label.ofElem_core trunk) of
				Nothing -> Node trunk_rendered children_rendered
				Just link -> Node trunk_rendered (Individual.render' link : children_rendered)

type ParseChildrenSituation =
	(Maybe (Data.Link Text), Forest (A Text) {- <- the rest of the children -})

parse_children :: Forest (A Text) -> Either ParseError ParseChildrenSituation
parse_children children =
	case children of
		[] -> Right (Nothing, [])
		first : rest ->
			maybe
				(Right (Nothing, children))
				(map (\ l -> (Just l, rest)))
				(Individual.parse' first)

parse :: Tree (A Text) -> Either ParseError (Tree (A Inline))
parse (Node trunk children) =
	let
		from_situation :: ParseChildrenSituation -> Either ParseError (Tree (A Inline))
		from_situation (l, rest_of_children) =
			map (Node (map (flip Data.Inline l) trunk)) (traverse parse rest_of_children)
		in parse_children children >>= from_situation

layer :: Optic.PartialIso' ParseError (Tree (A Text)) (Tree (A Inline))
layer = Optic.PartialIso render parse
