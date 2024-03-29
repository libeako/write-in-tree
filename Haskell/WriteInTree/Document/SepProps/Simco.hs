-- | serialization of the separate properties in simco language
module WriteInTree.Document.SepProps.Simco
(
	serialize,
)
where

import Data.Default.Class
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))
import Fana.Prelude
import Prelude (String)

import qualified Fana.Math.Algebra.Category.OnTypePairs as Category2
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.PropertyTree.Serialize as PropTree
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.AsAbstract as SimcoProps
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.AsText as SimcoAsText
import qualified Fana.Serial.Print.Show as Fana


type Text = String

prefix_simco_parse_error :: Text -> Text
prefix_simco_parse_error m = "error parsing SimCo:\n" <> Acc.extract (Fana.show m)

prefix_props_parse_error :: Text -> Text
prefix_props_parse_error m = "error parsing properties file:\n" <> m

serialize :: Default r => PropTree.RecordType r -> Optic.PartialIso' Text Text r
serialize rt =
	Category2.identity
	>**> Optic.piso_convert_error prefix_simco_parse_error SimcoAsText.serializer
	>**> Optic.to_PartialIso SimcoProps.serialize
	>**> Optic.piso_convert_error prefix_props_parse_error
		(PropTree.record_serializer_over_property_list rt)
