module WriteInTree.Document.File
(
	write, read,
)
where

import Data.Tree (Tree, Forest)
import Fana.Prelude
import Fana.Data.Tree.OfBase (children_in_tree)
import Prelude (Char, String, IO, FilePath)
import Technical.FolderMember (Folder, Reader, Member (..), member_string, read_forest)
import System.FilePath ((</>))
import WriteInTree.Document.Core.Serial.Page.Data
import WriteInTree.Document.Main (Document (..))
import WriteInTree.Document.SepProps.Data (DocSepProps (..))

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Tree as Tree
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Text.Indent as Tt
import qualified Fana.Serial.Print.Show as Fana
import qualified System.Directory as Directory
import qualified Technical.FolderMember as FolderMember
import qualified Technical.TextTree.MindMap as Mm
import qualified WriteInTree.Document.Core.Serial.Layers as CoreSerial
import qualified WriteInTree.Document.SepProps.Simco as SepPropsSimco


member_config :: Member DocSepProps
member_config =
	let
		render :: DocSepProps -> String
		render = SepPropsSimco.to_simco_text
		parse :: String -> Either String DocSepProps
		parse = 
			id
			>>> SepPropsSimco.parse_from_text
			>>> Bifunctor.first (Fana.show >>> Acc.extract >>> ("error in separate properties file:\n" <>))
		in
			FolderMember.lift_by_piso
				(Optic.PartialIso render parse)
				(member_string "separate properties [config]" "properties.simco.text")

member_content' :: DocSepProps -> Member Page
member_content' sep_props =
	let
		serializer :: Optic.PartialIso' String String Page
		serializer =
			let
				render :: DocSepProps -> Page -> String
				render config = Optic.down (CoreSerial.layer' config Tt.text_tree)
				parse :: DocSepProps -> String -> Either String Page
				parse config =
					id
					>>> Optic.piso_interpret (CoreSerial.layer' config Tt.text_tree)
					>>> Bifunctor.first (Fana.show >>> Acc.extract)
				in liftA2 Optic.PartialIso render parse sep_props
		in 
			FolderMember.lift_by_piso
				serializer
				(member_string "core page tree content" "content")

member_content :: DocSepProps -> Member Site
member_content sep_props =
	let
		serializer ::
			Fana.Showable Text e =>
			Optic.PartialIso' e Text [Tree Text] -> Optic.PartialIso' String String Site
		serializer tt_layer =
			let
				render :: DocSepProps -> Site -> String
				render config = Optic.down (CoreSerial.layer config tt_layer)
				parse :: DocSepProps -> String -> Either String Site
				parse config =
					id
					>>> Optic.piso_interpret (CoreSerial.layer config tt_layer)
					>>> Bifunctor.first (Fana.show >>> Acc.extract)
				in liftA2 Optic.PartialIso render parse sep_props
		mindmap_file_format :: FolderMember.FileFormat Site
		mindmap_file_format = FolderMember.FileFormat "mind-map" "tree.mm" (serializer Mm.layer)
		plain_text_tree_file_format :: FolderMember.FileFormat Site
		plain_text_tree_file_format = FolderMember.FileFormat "plain text tree" "content" (serializer Tt.text_tree)
		in FolderMember.member_multi_format "core tree content" (plain_text_tree_file_format, [mindmap_file_format])

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

single_folder_content_writer :: DocSepProps -> FilePath -> Page -> IO ()
single_folder_content_writer sep_props folder_path = 
	memberWriter (member_content' sep_props) folder_path

write_page_forest :: DocSepProps -> FilePath -> Forest (Folder Page) -> IO ()
write_page_forest sep_props folder_path = FolderMember.write_forest (single_folder_content_writer sep_props) folder_path

write :: FilePath -> Document -> IO ()
write address doc =
	let
		sep_props = docSepProps doc
		write_member :: Member d -> d -> IO ()
		write_member m = memberWriter m address
		pages_folder_path = address </> pages_folder_name
		folderify_page :: Page -> Folder Page
		folderify_page page = (title_of_page page, page)
		pages :: Forest (Folder Page)
		pages = map (map folderify_page) [docCore doc]
		in
			do
				Directory.createDirectory address
				Directory.createDirectory pages_folder_path
				write_member member_config (docSepProps doc)
				write_page_forest sep_props pages_folder_path pages

single_folder_content_reader :: DocSepProps -> Reader Site
single_folder_content_reader sep_props path = map Right (FolderMember.read path (member_content sep_props))

read_recursively :: FilePath -> DocSepProps -> IO (Forest Page)
read_recursively folder_path sep_props =
	let
		read_member :: Member Site -> IO (Site)
		read_member = FolderMember.read folder_path
		read_dir_to_page :: Folder Site -> Page
		read_dir_to_page (name, site) = 
			Tree.rootLabel (Optic.fill title_in_trunk_page (Optic.up file_name_iso name) site)
		in (map >>> map >>> map) read_dir_to_page (read_forest (single_folder_content_reader sep_props) folder_path)

read_single_folder :: FilePath -> IO Document
read_single_folder folder_path =
	let
		read_member :: Member d -> IO (d)
		read_member = FolderMember.read folder_path
		merge_sites :: Site -> Forest Page -> Site
		merge_sites main foldered_pages = Optic.fn_up children_in_tree (foldered_pages <>) main
		in
			do
				config <- read_member member_config
				core <- read_member (member_content config)
				foldered_pages <- read_recursively (folder_path </> pages_folder_name) config
				pure (Document config (merge_sites core foldered_pages))

read :: FilePath -> IO Document
read = read_single_folder
