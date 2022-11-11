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

import qualified Control.Monad.Except as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified System.Directory as Directory
import qualified Technical.FolderMember as FolderMember
import qualified WriteInTree.Document.Core.Serial.Layers as CoreSerial
import qualified WriteInTree.Document.SepProps.File as SepProps

type Text = Base.String


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
write address d =
	let
		sep_props = docSepProps d
		in
			do
				Directory.createDirectory address
				SepProps.write address sep_props
				memberWriter (member_core sep_props) address (docCore d)

read :: FilePath -> Monad.ExceptT Text IO Document
read folder_path =
	do
		config <- SepProps.read folder_path
		core <- Monad.ExceptT (memberReader (member_core config) folder_path)
		pure (Document config core)
