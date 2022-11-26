module WriteInTree.Document.File
(
	write, read,
)
where

import Control.Monad.Except (ExceptT (..), liftEither)
import Data.Tree (Tree, Forest)
import Fana.Prelude
import Prelude (Char, String, IO, FilePath)
import Technical.FolderMember (Folder, Reader, Member (..), member_string, read_forest)
import System.FilePath ((</>))
import WriteInTree.Document.Core.Serial.Page.Data
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (PageAddress (..))
import WriteInTree.Document.Main (Document (..))
import WriteInTree.Document.SepProps.Data (DocSepProps (..), FolderSepProps (FolderSepProps))

import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Data.Key.LensToMaybeElement as MapI
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Data.Key.Map.KeyIsString as SMap
import qualified Fana.Data.List as List
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified System.Directory as Directory
import qualified Technical.FolderMember as FolderMember
import qualified WriteInTree.Document.Core.Serial.Layers as CoreSerial
import qualified WriteInTree.Document.SepProps.Data as SepPropsData
import qualified WriteInTree.Document.SepProps.PropTree as SepPropsPT
import qualified WriteInTree.Document.SepProps.Simco as SepPropsSimco


member_config :: Member DocSepProps
member_config =
	let
		serializer :: Optic.PartialIso' Text Text DocSepProps
		serializer =
			Optic.piso_convert_error
				(Fana.show >>> Acc.extract >>> ("error in document's separate properties file:\n" <>))
				(SepPropsSimco.layer SepPropsPT.type_structure_doc_sep_props)
		in
			FolderMember.lift_by_piso serializer
				(member_string "document separate properties" "properties.simco")

member_folder_config :: Member FolderSepProps
member_folder_config =
	let
		serializer :: Optic.PartialIso' Text Text FolderSepProps
		serializer =
			Optic.piso_convert_error
				(Fana.show >>> Acc.extract >>> ("error in folder's separate properties file:\n" <>))
				(SepPropsSimco.layer SepPropsPT.type_structure_folder_sep_props)
		in
			FolderMember.lift_by_piso serializer
				(member_string "folder separate properties" "properties.simco")

member_content :: Member PageContentBulk
member_content =
	let
		serializer :: Optic.PartialIso' String String PageContentBulk
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
single_folder_content_writer folder_path page =
	do
		memberWriter member_folder_config folder_path (FolderSepProps (fst page))
		memberWriter member_content folder_path (snd (snd page))

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

single_folder_content_reader :: Reader (PageAddress, PageContentBulk)
single_folder_content_reader path =
	do
		sep_props <- FolderMember.memberReader member_folder_config path
		page_content_bulk <- FolderMember.memberReader member_content path
		pure (SepPropsData.address sep_props, page_content_bulk)

read_recursively :: FilePath -> ExceptT String IO (Forest Page)
read_recursively folder_path =
	let
		read_dir_to_page :: Folder (PageAddress, PageContentBulk) -> Page
		read_dir_to_page (name, (address, content_bulk)) = (address, (name, content_bulk))
		in (map >>> map >>> map) read_dir_to_page (read_forest single_folder_content_reader folder_path)

search_error_in_site :: Site -> Maybe String
search_error_in_site site =
	let
		page_map_result :: Either (String, [PageContent]) (SMap.Map Char PageContent)
		page_map_result = MapI.from_list_of_uniques (map (Bifunctor.first unwrapPageAddress) (toList site))
		in
			case page_map_result of
				Left (repeted_address, _) -> Just ("multiple pages have the same address " <> repeted_address)
				Right page_map -> 
					let
						addresses :: [String]
						addresses = Optic.to_list internal_address_in_link_in_site site
						reference_error_of_address :: String -> Maybe String
						reference_error_of_address address = 
							maybe (Just ("page with address " <> address <> " does not exist")) (const Nothing)
								(flip MapI.get_at page_map address)
						in List.first (catMaybes (map reference_error_of_address addresses))

read :: FilePath -> ExceptT Text IO Document
read folder_path =
	let
		read_member :: Member d -> ExceptT Text IO d
		read_member = FolderMember.read folder_path
		process_page_forest :: Forest Page -> Either Text (Tree Page)
		process_page_forest = 
			\case
				[single] -> maybe (Right single) Left (search_error_in_site single)
				_ -> Left "page folder forest must consist of a single tree"
		in
			do
				sep_props <- read_member member_config
				foldered_pages <- read_recursively (folder_path </> pages_folder_name)
				liftEither (map (Document sep_props)(process_page_forest foldered_pages))
