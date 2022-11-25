module WriteInTree.Document.SepProps.PropTree
(
	type_structure,
)
where

import Fana.Prelude
import WriteInTree.Document.SepProps.Data (DocSepProps (..))

import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.PropertyTree.Serialize as PropTree
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.LanguageVersion as Version
import qualified WriteInTree.Document.SepProps.Data as Props


type Char = Base.Char
type Text = [Char]

type_structure :: PropTree.RecordType DocSepProps
type_structure = 
	let
		field_version :: PropTree.Field DocSepProps
		field_version = 
			let
				version_serializer :: PropTree.Serializer Version.Version
				version_serializer = 
					PropTree.atomic_serializer 
						(Optic.PartialIso Base.show (Version.version_from_text >>> Bifunctor.first Base.show))
				in PropTree.Field "language-version" Props.lens_lang_ver_in_props version_serializer
		in MapI.from_list 
			[ ("language-version", field_version)
			]
