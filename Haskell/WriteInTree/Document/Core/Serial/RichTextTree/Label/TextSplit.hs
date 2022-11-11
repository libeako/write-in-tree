module WriteInTree.Document.Core.Serial.RichTextTree.Label.TextSplit
(
	ClassName, Class (..), Configuration, 
	L, H, layer, test_layer,
)
where

import Data.Functor.Identity (Identity (..))
import Fana.Develop.Test.Define (Test)
import Fana.Prelude
import WriteInTree.Document.SepProps.Data (InlineClass (..))

import qualified Control.Monad.State.Strict as Mtl
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Base
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Fana.Data.Filter as Filter
import qualified Fana.Data.HeteroPair as Pair
import qualified Fana.Data.Key.LensToMaybeElement as LensAt
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Data.Key.Map.KeyIsString as MapS
import qualified Fana.Data.List as List
import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Optic.Concrete.Categories.Iso as Optic
import qualified Prelude as Base

type Char = Base.Char
type Text = [Char]
type InlineContent = Text
type ClassName = Text
type ClassCode = Text
type L = Text
type H = ([ClassName], InlineContent)
type Configuration = [Class]

type Class = InlineClass

class_to_key_value_pair :: Class -> (ClassName, ClassCode)
class_to_key_value_pair c = (ilc_name c, ilc_code c)

parse_single :: Class -> InlineContent -> (Maybe ClassName, InlineContent)
parse_single c i = Base.maybe (Nothing, i) (Pair.after (Just (ilc_name c))) (List.stripPrefix (ilc_code c) i)

render_single :: ClassCode -> InlineContent -> InlineContent
render_single = (<>)

make_map_name_code :: Configuration -> MapS.Map Char Text
make_map_name_code = id 
	>>> map class_to_key_value_pair 
	>>> MapI.from_list_lists 
	>>> Filter.map_filter List.first

parse_all :: Configuration -> L -> H
parse_all possible_classes l = 
	let
		state_transformer :: Class -> Mtl.State InlineContent (Maybe InlineContent)
		state_transformer c = Mtl.StateT (parse_single c >>> Identity)
		in Bifunctor.first Base.catMaybes (Mtl.runState (traverse state_transformer possible_classes) l)

render_all :: MapS.Map Char Text -> H -> L
render_all map_name_code (cns, i) = let
	codes :: [ClassCode]
	codes = Base.catMaybes (map (flip LensAt.get_at map_name_code) cns)
	in Base.foldr' render_single i codes

layer :: Configuration -> Optic.Iso' L H
layer config = Optic.Iso (render_all (make_map_name_code config)) (parse_all config)


---------------------- tests ----------------------------

test_layer :: Test
test_layer = let
	t = "hello"
	config = [InlineClass "page" "*->", InlineClass "title" "***"]
	highs = [([], t), (map ilc_name config, t)]
	in Test.single "text-split" (Optic.test_iso_law (layer config) (highs, []))
