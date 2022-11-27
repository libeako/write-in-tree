module WriteInTree.Document.SepProps.PropTree
(
	type_structure_doc_sep_props,
	type_structure_folder_sep_props,
)
where

import Fana.Prelude
import WriteInTree.Document.Core.Data (PageAddress (..))
import WriteInTree.Document.SepProps.Data (DocSepProps (..), FolderSepProps (..))

import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.PropertyTree.Serialize as PropTree
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.LanguageVersion as Version
import qualified WriteInTree.Document.SepProps.Data as Props


type Char = Base.Char
type Text = [Char]

type_structure_doc_sep_props :: PropTree.RecordType DocSepProps
type_structure_doc_sep_props = 
	let
		field_version :: PropTree.Field DocSepProps
		field_version = 
			let
				version_serializer :: PropTree.Serializer Version.Version
				version_serializer = 
					PropTree.atomic_serializer 
						(Optic.PartialIso Base.show (Version.version_from_text >>> Bifunctor.first Base.show))
				in PropTree.Field "language-version" Props.lang_ver_in_props version_serializer
		in MapI.from_list 
			[ ("language-version", field_version)
			]

type_structure_folder_sep_props :: PropTree.RecordType FolderSepProps
type_structure_folder_sep_props = 
	let
		field_address :: PropTree.Field FolderSepProps
		field_address = 
			let
				address_serializer :: PropTree.Serializer PageAddress
				address_serializer = 
					PropTree.atomic_serializer 
						(
							Optic.PartialIso unwrapPageAddress (PageAddress >>> pure)
						)
				in PropTree.Field "address" Props.address_in_props address_serializer
		in MapI.from_list 
			[ ("address", field_address)
			]
