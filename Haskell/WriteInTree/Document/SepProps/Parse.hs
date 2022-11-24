module WriteInTree.Document.SepProps.Parse
(
	parse_from_line_forest,
)
where

import Fana.Prelude
import Fana.Serial.Bidir.Instances.Text.PropertyTree.Data
import WriteInTree.Document.SepProps.Data (DocSepProps(..))

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Tree as Base
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.Data as SimcoDL
import qualified Fana.Serial.Print.Show as Fana
import qualified Technical.ParsePropertyTree as PropTree
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.LanguageVersion as Version
import qualified WriteInTree.Document.SepProps.Data as Props


type Char = Base.Char
type Text = [Char]

type_structure :: PropTree.RecordType DocSepProps
type_structure = 
	let
		field_version :: PropTree.HiddenFieldOfProduct DocSepProps
		field_version = 
			let
				version_parser :: PropTree.Parser Version.Version
				version_parser = \case
					PropertyValueAtomic version_text -> 
						map const (Bifunctor.first Fana.show (Version.version_from_text version_text))
					PropertyValueComposite _ -> Left "expected single element but found composite"
				in PropTree.field_from_optic Props.lens_lang_ver_in_props version_parser
		in MapI.from_list 
			[ ("language-version", field_version)
			]


parse_from_line_forest :: Base.Forest SimcoDL.NodeWithActivity -> Either (Accu.Accumulated Text) DocSepProps
parse_from_line_forest = 
	let
		modifier :: Base.Forest SimcoDL.NodeWithActivity -> Either (Accu.Accumulated Text) (DocSepProps -> DocSepProps)
		modifier = id
			>>> SimcoDL.clean
			>>> PropertyValueComposite
			>>> PropTree.parser_of_record type_structure
	in 
		modifier >>> map ($ def)
