module Technical.TextTree.General
(
	ParseError, forest_to_tree_serializer,
)
where

import Data.Tree (Tree)
import Fana.Prelude

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base


type Text = Base.String


data ParseError fe = ForestParseError fe | NotTree
	
instance Fana.Showable Text fe => Fana.Showable Text (ParseError fe) where
	show = 
		\case
			ForestParseError fe -> Fana.show fe
			NotTree -> "not tree [multiple roots]"


extract_single :: [a] -> Either (ParseError e) a
extract_single =
	\case
		[t] -> Right t
		_ -> Left NotTree

forest_to_tree :: Either fe [Tree e] -> Either (ParseError fe) (Tree e)
forest_to_tree = either (ForestParseError >>> Left) extract_single

forest_to_tree_serializer :: 
	Optic.PartialIso fe l1 l2 [Tree e] [Tree e] -> 
	Optic.PartialIso (ParseError fe) l1 l2 [Tree e] (Tree e)
forest_to_tree_serializer (Optic.PartialIso down up) = 
	Optic.PartialIso down (up >>> forest_to_tree)
