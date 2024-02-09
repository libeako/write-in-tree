module WriteInTree.Document.File
(
	write, read,
)
where

import Control.Monad.Except (ExceptT (..), liftEither, throwError)
import Data.Tree (Tree, Forest)
import Fana.Prelude
import Prelude (Char, String, IO, FilePath)
import Technical.FolderMember (Folder, Reader, Member (..), member_string, read_forest)
import System.FilePath ((</>))
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Main (Document (..))

import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Data.Function as Fn
import qualified Fana.Data.Key.LensToMaybeElement as MapI
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Data.Key.Map.KeyIsString as SMap
import qualified Fana.Data.List as List
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified System.Directory as Directory
import qualified Technical.FolderMember as FolderMember
import qualified WriteInTree.Document.Core.Serial.All as CoreSerial


member_content :: Member StructureAsForest
member_content =
	let
		serializer :: Optic.PartialIso' String String StructureAsForest
		serializer = Optic.piso_convert_error (Fana.show >>> Acc.extract) CoreSerial.serialize
		in
			FolderMember.lift_by_piso
				serializer
				(member_string "core page tree content" "_content")

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
	memberWriter member_content folder_path (snd (snd page))

write_page_forest :: FilePath -> Forest (Folder Page) -> IO ()
write_page_forest folder_path =
	FolderMember.write_forest single_folder_content_writer folder_path

write :: FilePath -> Document -> IO ()
write address doc =
	let
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
				write_page_forest pages_folder_path pages

single_folder_content_reader :: Reader (Address, StructureAsForest)
single_folder_content_reader path =
	do
		page_content_bulk <- FolderMember.memberReader member_content path
		address <-
			case fst page_content_bulk of
				Nothing -> throwError (path <> ": error:\n File does not contain main identifier.")
				Just a -> pure a
		pure (address, page_content_bulk)

read_recursively :: FilePath -> ExceptT String IO (Forest Page)
read_recursively folder_path =
	let
		read_dir_to_page :: Folder (Address, StructureAsForest) -> Page
		read_dir_to_page (folder_name, (address, content_bulk)) = 
			let
				smuggle_in_address :: Fn.Endo StructureAsForest 
				smuggle_in_address = Bifunctor.first (const (Just address))
				in (address, (Optic.ofIso_up file_name_iso folder_name, smuggle_in_address content_bulk))
		in (map >>> map >>> map) read_dir_to_page (read_forest single_folder_content_reader folder_path)

search_page_address_error_in_site :: Site -> Maybe String
search_page_address_error_in_site site =
	let
		page_map_result :: Either (String, [PageContent]) (SMap.Map Char PageContent)
		page_map_result = MapI.from_list_of_uniques (map (Bifunctor.first unwrapPageAddress) (toList site))
		in
			case page_map_result of
				Left (repeted_address, _) -> Just ("multiple pages have the same address " <> repeted_address)
				Right page_map -> 
					let
						addresses :: [String]
						addresses = Optic.to_list internal_address_in_Link_in_Site site
						reference_error_of_address :: String -> Maybe String
						reference_error_of_address address = 
							maybe (Just ("page with address " <> address <> " does not exist")) (const Nothing)
								(flip MapI.get_at page_map address)
						in List.first (catMaybes (map reference_error_of_address addresses))

read :: FilePath -> ExceptT Text IO Document
read folder_path =
	let
		process_page_forest :: Forest Page -> Either Text (Tree Page)
		process_page_forest = 
			\case
				[single] -> maybe (Right single) Left (search_page_address_error_in_site single)
				_ -> Left "page folder forest must consist of a single tree"
		in
			do
				foldered_pages <- read_recursively (folder_path </> pages_folder_name)
				liftEither (map Document (process_page_forest foldered_pages))
