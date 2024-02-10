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
import qualified Fana.Data.HeteroPair as Pair
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
				Nothing -> throwError ("Error: File " <> path <> " does not contain main identifier.")
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


read :: FilePath -> ExceptT Text IO (SiteAddressMap, Document)
read =
	let
		read_page_forest :: FilePath -> ExceptT String IO (Forest Page)
		read_page_forest = ( </> pages_folder_name) >>> read_recursively
		singlify_forest :: Forest Page -> Either Text (Tree Page)
		singlify_forest = 
			\ case
				[single] -> Right single
				_ -> Left "page folder forest must consist of a single tree"
		attach_address_map :: Tree Page -> Either Text (SiteAddressMap, Document)
		attach_address_map t = map (Pair.before (Document t)) (address_map t)
		from_page_forest :: Forest Page -> Either Text (SiteAddressMap, Document)
		from_page_forest = singlify_forest >=> attach_address_map
		in read_page_forest >=> (from_page_forest >>> liftEither)
