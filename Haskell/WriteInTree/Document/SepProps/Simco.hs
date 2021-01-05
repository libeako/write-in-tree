-- | serialization of the separate document properties in simco language
module WriteInTree.Document.SepProps.Simco
(
	ParseError,
	to_simco_text, parse_from_text,
	test,
)
where

import Data.Tree (Forest)
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Develop.Test.Define (Test)
import Fana.Prelude
import Prelude (String)
import WriteInTree.Document.SepProps.Data

import qualified Data.Default.Class as Default
import qualified Data.Tree as Base
import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataLines as SimcoLow -- low level data
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Layer as Simco
import qualified Fana.Serial.Print.Show as Fana
import qualified WriteInTree.Document.SepProps.Parse as Parse


type Text = String

-- | renders the given data into simco language.
to_simco :: DocSepProps -> Forest SimcoLow.NodeWithActivity
to_simco props = 
	[
		Base.Node (SimcoLow.Active, (SimcoLow.make_atom "language-version" (Accu.extract (Fana.show (language_version props))))) []
	]

data ParseError
	= ParseErrorInSimcoLayer Simco.ParseError
	| ParseErrorInUpperLayer (Accu.Accumulated Text)

instance Fana.Showable Text ParseError where
	show = \case
		ParseErrorInSimcoLayer details -> "error parsing SimCo : " <> Fana.show details
		ParseErrorInUpperLayer details -> "error parsing layer above SimCo : " <> Fana.show details

upper_layer :: Optic.PartialIso' (Accu.Accumulated Text) (Base.Forest SimcoLow.NodeWithActivity) DocSepProps
upper_layer = Optic.PartialIso to_simco Parse.parse_from_line_forest

whole_layer :: Optic.PartialIso' ParseError Text DocSepProps
whole_layer = 
	(Optic.piso_convert_error ParseErrorInSimcoLayer Simco.layer) >**>
	(Optic.piso_convert_error ParseErrorInUpperLayer upper_layer)

to_simco_text :: DocSepProps -> Text
to_simco_text = Optic.down whole_layer

parse_from_text :: Text -> Either ParseError DocSepProps
parse_from_text = Optic.piso_interpret whole_layer


-- * test :

test_simco_layer :: Test
test_simco_layer = 
	Test.single "simco layer"
		(Optic.test_piso (Category2.empty, Category2.empty) [] [to_simco Default.def] Simco.layer)

test_upper_layer :: Test
test_upper_layer = 
	Test.single "upper layer"
		(Optic.test_piso (Category2.empty, Category2.empty) [] [Default.def] upper_layer)

test_layer :: Test
test_layer = 
	Test.single "whole layer"
		(Optic.test_piso (Category2.empty, Category2.empty) [] [Default.def] whole_layer)

test :: Test
test = Test.bunch "document : separate properties : simco" [test_simco_layer, test_upper_layer, test_layer]
