-- | serialization of the separate document properties in simco language
module WriteInTree.Document.SepProps.Simco
(
	ParseError,
	to_simco_text, parse_from_text,
	test,
)
where

import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Develop.Test.Define (Test)
import Fana.Prelude
import Prelude (String)
import WriteInTree.Document.SepProps.Data

import qualified Prelude as Base
import qualified Data.Default.Class as Default
import qualified Data.Tree as Base
import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data as SimcoData
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.IndentedTextSerial as SimcoSerial
import qualified Fana.Serial.Print.Show as Fana
import qualified WriteInTree.Document.SepProps.Parse as Parse


type Text = String

from_InlineClass_to_simco :: InlineClass -> SimcoData.Tree
from_InlineClass_to_simco (InlineClass name codes) = 
	SimcoData.make_tree "-" 
		[
		SimcoData.make_atom "name" name,
		SimcoData.make_tree "codes" (map (SimcoData.make_atom "-") codes)
		]

-- | renders the given data into simco language.
to_simco :: DocSepProps -> SimcoData.Forest
to_simco props = 
	[
		SimcoData.make_atom "language-version" (Base.show (language_version props)),
		SimcoData.make_tree "inline-classes" (map from_InlineClass_to_simco (prop_inline_classes props))
	]

data ParseError
	= ParseErrorInSimcoLayer SimcoSerial.ParseError
	| ParseErrorInUpperLayer (Accu.Accumulated Text)

instance Fana.Showable Text ParseError where
	show = \case
		ParseErrorInSimcoLayer details -> "error parsing SimCo:\n" <> Fana.show details
		ParseErrorInUpperLayer details -> "error parsing layer above SimCo:\n" <> Fana.show details

upper_layer :: Optic.PartialIso' (Accu.Accumulated Text) (Base.Forest SimcoData.NodeWithActivity) DocSepProps
upper_layer = Optic.PartialIso to_simco Parse.parse_from_line_forest

whole_layer :: Optic.PartialIso' ParseError Text DocSepProps
whole_layer = 
	(Optic.piso_convert_error ParseErrorInSimcoLayer SimcoSerial.serializer) >**>
	(Optic.piso_convert_error ParseErrorInUpperLayer upper_layer)

to_simco_text :: DocSepProps -> Text
to_simco_text = Optic.down whole_layer

parse_from_text :: Text -> Either ParseError DocSepProps
parse_from_text = Optic.piso_interpret whole_layer


-- * test :

test_simco_layer :: Test
test_simco_layer = 
	Test.single "simco layer"
		(
		Optic.test_piso (Category2.identity, Category2.identity)
		[] [to_simco Default.def] SimcoSerial.serializer
		)

test_upper_layer :: Test
test_upper_layer = 
	Test.single "upper layer"
		(Optic.test_piso (Category2.identity, Category2.identity) [] [Default.def] upper_layer)

test_layer :: Test
test_layer = 
	Test.single "whole layer"
		(Optic.test_piso (Category2.identity, Category2.identity) [] [Default.def] whole_layer)

test :: Test
test = Test.bunch "document : separate properties : simco" [test_simco_layer, test_upper_layer, test_layer]
