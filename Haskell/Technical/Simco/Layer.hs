-- | serialization of the separate document properties in simco language
module Technical.Simco.Layer
(
	ParseError,
	layer,
)
where

import Fana.Haskell.DescribingClass
import Fana.Haskell.TypePair (Fst, Snd)
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import Prelude (Int, (+), Char, String)

import qualified Data.Foldable as Base
import qualified Data.Functor.Compose as Base
import qualified Data.List as List
import qualified Data.Traversable as Traversable
import qualified Data.Tree as Base
import qualified Fana.Data.Tree.SerializeHight as TreeSerial
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Parse as Serial
import qualified Fana.Serial.Bidir.Serializer as Serial
import qualified Fana.Serial.Print.Show as Fana
import qualified Fana.Serial.Print.Show as Show
import qualified Technical.LineTree as LineTree
import qualified Technical.Simco.DataLines as SimcoDL
import qualified Technical.Simco.Lines as Lines


type Text = String

data LineIdentifier = LineIdentifier { line_ordinal :: Int, line_content :: Text }

instance Fana.Showable Text LineIdentifier where
	show l = 
		mempty
		<> "line " <> Show.from_Show (line_ordinal l) 
		<> " [\"" <> Accu.single (line_content l) <> "\"]"

data ParseError
	= ErrorInHightListParsing (TreeSerial.HightListParseError Text)
	| ErrorInLineParsing LineIdentifier [Serial.Error Char]

instance Fana.Showable Text ParseError where
	show = \case
		ErrorInHightListParsing details -> "error in hight list parsing : " <> Fana.show details
		ErrorInLineParsing line errors -> 
			mempty
			<> "error in line parsing in " <> Fana.show line 
			<> " : ["
			<> Base.fold (List.intersperse ", " (map Fana.show errors))
			<> "]"

type Layer l h = Optic.PartialIso ParseError (Fst l) (Snd l) (Fst h) (Snd h)
type Layer' l h = Layer '(l, l) '(h, h)


layer_indentation :: Layer' Text [(TreeSerial.Hight, Text)]
layer_indentation = convert_from_describing_class_4 LineTree.coding_lines

layer_tree_structure :: Layer' [(TreeSerial.Hight, Text)] [Base.Tree Text]
layer_tree_structure = Optic.piso_convert_error ErrorInHightListParsing TreeSerial.serializer

layer_text_tree :: Layer' Text [Base.Tree Text]
layer_text_tree = layer_indentation >**> layer_tree_structure

layer_line :: Optic.PartialIso' [Serial.Error Char] Text SimcoDL.Node
layer_line = Serial.to_partial_iso (Lines.serializer_line)

layer_identified_line :: Layer '(Text, (LineIdentifier, Text)) '(SimcoDL.Node, SimcoDL.Node)
layer_identified_line = Optic.piso_convert_error (uncurry ErrorInLineParsing) (Optic.add_for_failure layer_line)

layer_lines_general :: Traversable t => Layer '(t Text, t (LineIdentifier, Text)) '(t SimcoDL.Node, t SimcoDL.Node)
layer_lines_general = Optic.lift_piso layer_identified_line

identify_lines :: Traversable t => t Text -> t (LineIdentifier, Text)
identify_lines = 
	let
		accumulator :: Int -> Text -> (Int, (LineIdentifier, Text))
		accumulator count text = (count +1, ((LineIdentifier count text), text))
		in Traversable.mapAccumL accumulator 1 >>> snd

layer_lines :: 
	Layer 
		'(Text, Text)
		'(Base.Compose [] Base.Tree Text, Base.Compose [] Base.Tree (LineIdentifier, Text))
layer_lines = 
	Optic.piso_convert_all id id (Base.Compose >>> identify_lines) Base.getCompose id
	layer_text_tree

layer :: Layer' Text (Base.Forest SimcoDL.Node)
layer = 
	Optic.piso_convert_all id id Base.getCompose Base.Compose id
	(layer_lines >**> layer_lines_general)

