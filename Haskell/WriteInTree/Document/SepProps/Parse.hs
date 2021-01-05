module WriteInTree.Document.SepProps.Parse
(
	parse_from_line_forest,
)
where

import Fana.Prelude
import WriteInTree.Document.Core.Serial.LanguageVersion (Version (..))
import WriteInTree.Document.SepProps.Data (DocSepProps(..))

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Tree as Base
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Data.Key.Map.KeyIsString as StringyMap
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Data as PropTree
import qualified Fana.Serial.Bidir.Instances.Text.PropertyTree.Simco.DataLines as SimcoDL
import qualified Fana.Serial.Print.Show as Fana
import qualified Technical.ParsePropertyTree as PropTree
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.LanguageVersion as Version
import qualified WriteInTree.Document.SepProps.Data as Props


type Char = Base.Char
type Text = [Char]

type_structure :: PropTree.RecordType DocSepProps
type_structure = let
	field_version :: PropTree.HiddenFieldOfProduct DocSepProps
	field_version = let
		version_verifier :: Version -> Either (Accu.Accumulated Text) Version
		version_verifier v = 
			if v == Version 1 1 then Right v 
				else Left "reading this language version is not supported"
		version_parser :: PropTree.Parser Version.Version
		version_parser = \case
			PropTree.Single version_text -> 
				map const
					(
						Bifunctor.first Fana.show (Version.version_from_text version_text)
						>>= version_verifier
					)
			PropTree.Composite _ -> Left "expected single element but found composite"
		in PropTree.field_from_optic Props.lens_lang_ver_in_props version_parser
	field_inline_classes :: PropTree.HiddenFieldOfProduct DocSepProps
	field_inline_classes = let
		parser_of_InlineClass :: PropTree.Parser Props.InlineClass
		parser_of_InlineClass = let
			field_of_InlineClass_name :: PropTree.HiddenFieldOfProduct Props.InlineClass
			field_of_InlineClass_name = PropTree.field_from_optic Props.ofInlineClass_name PropTree.parser_of_text
			field_of_InlineClass_code :: PropTree.HiddenFieldOfProduct Props.InlineClass
			field_of_InlineClass_code = PropTree.field_from_optic Props.ofInlineClass_code PropTree.parser_of_text
			field_map :: StringyMap.Map Char (PropTree.HiddenFieldOfProduct Props.InlineClass)
			field_map = MapI.from_list 
				[ ("name", field_of_InlineClass_name)
				, ("code", field_of_InlineClass_code)
				]
			in PropTree.parser_of_record field_map
		in PropTree.field_from_optic Props.ofProps_inline_classes (PropTree.parser_of_list def parser_of_InlineClass)
	in MapI.from_list 
		[ ("language-version", field_version)
		, ("inline-classes", field_inline_classes)
		]


parse_from_line_forest :: Base.Forest SimcoDL.NodeWithActivity -> Either (Accu.Accumulated Text) DocSepProps
parse_from_line_forest = 
	let
		modifier :: Base.Forest SimcoDL.NodeWithActivity -> Either (Accu.Accumulated Text) (DocSepProps -> DocSepProps)
		modifier = id
			>>> SimcoDL.to_props
			>>> PropTree.Composite
			>>> PropTree.parser_of_record type_structure
	in 
		modifier >>> map ($ def)
