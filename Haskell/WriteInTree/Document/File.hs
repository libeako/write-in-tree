module WriteInTree.Document.File
(
	write, read,
)
where

import Fana.Prelude
import Prelude (String, IO, FilePath)
import Technical.FolderMember (Member (..), member_string)
import WriteInTree.Document.Core.Serial.Page.Main (Site)
import WriteInTree.Document.Main (Document (..))
import WriteInTree.Document.SepProps.Data (DocSepProps (..))

import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified System.Directory as Directory
import qualified Technical.FolderMember as FolderMember
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

member_core :: DocSepProps -> Member Site
member_core sep_props =
	let
		render :: DocSepProps -> Site -> String
		render config = Optic.down (CoreSerial.layer config)
		parse :: DocSepProps -> String -> Either String Site
		parse config =
			id
			>>> Optic.piso_interpret (CoreSerial.layer config) 
			>>> Bifunctor.first (Fana.show >>> Acc.extract)
		in
			FolderMember.lift_by_piso
				(Optic.PartialIso (render sep_props) (parse sep_props))
				(member_string "core tree content" "tree.mm")

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
				write_member (member_core sep_props) (docCore doc)

read :: FilePath -> IO Document
read folder_path =
	let
		read_member :: Member d -> IO (d)
		read_member = FolderMember.read folder_path
		in
			do
				config <- read_member member_config
				core <- read_member (member_core config)
				pure (Document config core)
