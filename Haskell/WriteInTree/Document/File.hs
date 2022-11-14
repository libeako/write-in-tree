module WriteInTree.Document.File
(
	write, read,
)
where

import Data.Tree (Tree)
import Fana.Prelude
import Prelude (String, IO, FilePath)
import Technical.FolderMember (Member (..), member_string)
import WriteInTree.Document.Core.Serial.Page.Main (Site)
import WriteInTree.Document.Main (Document (..))
import WriteInTree.Document.SepProps.Data (DocSepProps (..))

import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Bidir.Instances.Text.Indent as Tt
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified System.Directory as Directory
import qualified Technical.FolderMember as FolderMember
import qualified Technical.TextTree.MindMap as Mm
import qualified WriteInTree.Document.Core.Serial.Layers as CoreSerial
import qualified WriteInTree.Document.SepProps.Simco as SepPropsSimco

type Text = Base.String


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
		in FolderMember.member_multi_format "core tree content" (mindmap_file_format, [plain_text_tree_file_format])

write :: FilePath -> Document -> IO ()
write address doc =
	let
		sep_props = docSepProps doc
		write_member :: Member d -> d -> IO ()
		write_member m = memberWriter m address
		in
			do
				Directory.createDirectory address
				write_member member_config (docSepProps doc)
				write_member (member_content sep_props) (docCore doc)

read :: FilePath -> IO Document
read folder_path =
	let
		read_member :: Member d -> IO (d)
		read_member = FolderMember.read folder_path
		in
			do
				config <- read_member member_config
				core <- read_member (member_content config)
				pure (Document config core)
