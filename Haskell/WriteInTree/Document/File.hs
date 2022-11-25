module WriteInTree.Document.File
(
	write, read,
)
where

import Data.Tree (Tree, Forest)
import Fana.Prelude
import Prelude (Char, String, IO, FilePath)
import Technical.FolderMember (Folder, Reader, Member (..), member_string, read_forest)
import System.FilePath ((</>))
import WriteInTree.Document.Core.Serial.Page.Data
import WriteInTree.Document.Main (Document (..))
import WriteInTree.Document.SepProps.Data (DocSepProps (..))

import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified System.Directory as Directory
import qualified Technical.FolderMember as FolderMember
import qualified WriteInTree.Document.Core.Serial.Layers as CoreSerial
import qualified WriteInTree.Document.SepProps.PropTree as SepPropsPT
import qualified WriteInTree.Document.SepProps.Simco as SepPropsSimco


member_config :: Member DocSepProps
member_config =
	let
		serializer :: Optic.PartialIso' Text Text DocSepProps
		serializer =
			Optic.piso_convert_error
				(Fana.show >>> Acc.extract >>> ("error in separate properties file:\n" <>))
				(SepPropsSimco.layer SepPropsPT.type_structure)
		in
			FolderMember.lift_by_piso serializer
				(member_string "separate properties [config]" "properties.simco")

member_content :: Member Page
member_content =
	let
		serializer :: Optic.PartialIso' String String Page
		serializer = Optic.piso_convert_error (Fana.show >>> Acc.extract) CoreSerial.layer
		in 
			FolderMember.lift_by_piso
				serializer
				(member_string "core page tree content" "content")

file_char_iso :: Optic.Iso' Char Char
file_char_iso = 
	let
		r c = if c == ' ' then '_' else c
		p c = if c == '_' then ' ' else c
		in Optic.Iso r p

file_name_iso :: Optic.Iso' String String
file_name_iso = Optic.lift_iso file_char_iso

pages_folder_name :: Text
pages_folder_name = "pages"

single_folder_content_writer :: FilePath -> Page -> IO ()
single_folder_content_writer folder_path = memberWriter member_content folder_path

write_page_forest :: FilePath -> Forest (Folder Page) -> IO ()
write_page_forest folder_path = 
	FolderMember.write_forest single_folder_content_writer folder_path

write :: FilePath -> Document -> IO ()
write address doc =
	let
		sep_props = docSepProps doc
		write_member :: Member d -> d -> IO ()
		write_member m = memberWriter m address
		pages_folder_path = address </> pages_folder_name
		folderify_page :: Page -> Folder Page
		folderify_page page = (Optic.down file_name_iso (title_of_page page), page)
		pages :: Forest (Folder Page)
		pages = map (map folderify_page) [docCore doc]
		in
			do
				Directory.createDirectory address
				Directory.createDirectory pages_folder_path
				write_member member_config (docSepProps doc)
				write_page_forest pages_folder_path pages

single_folder_content_reader :: Reader Page
single_folder_content_reader path = 
	map Right (FolderMember.read path member_content)

read_recursively :: FilePath -> IO (Forest Page)
read_recursively folder_path =
	let
		read_member :: Member Site -> IO (Site)
		read_member = FolderMember.read folder_path
		read_dir_to_page :: Folder Page -> Page
		read_dir_to_page (name, page) = Optic.fill title_in_page (Optic.up file_name_iso name) page
		in (map >>> map >>> map) read_dir_to_page (read_forest single_folder_content_reader folder_path)

read :: FilePath -> IO Document
read folder_path =
	let
		read_member :: Member d -> IO (d)
		read_member = FolderMember.read folder_path
		treeify_page_forest :: Forest p -> Tree p
		treeify_page_forest = \case
			[single] -> single
			_ -> Base.error "page folder forest must consist of a single tree"
		in
			do
				config <- read_member member_config
				foldered_pages <- read_recursively (folder_path </> pages_folder_name)
				pure (Document config (treeify_page_forest foldered_pages))
