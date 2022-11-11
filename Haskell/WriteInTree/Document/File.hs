module WriteInTree.Document.File
(
	write, read,
)
where

import Fana.Prelude
import Prelude (String, IO, FilePath)
import System.FilePath ((</>))
import Technical.FolderMember (Member (..))
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
import qualified WriteInTree.Document.Core.Serial.Layers as CoreSerial
import qualified WriteInTree.Document.SepProps.File as SepProps
import qualified WriteInTree.Document.SepProps.Simco as SepPropsSimco


type Text = Base.String


sep_props_file_name :: String
sep_props_file_name = "properties.simco.text"

render_sep_props :: DocSepProps -> String
render_sep_props = SepPropsSimco.to_simco_text

member_core :: DocSepProps -> Member Site
member_core sep_props = 
	let
		file_name = "tree.mm"
		writer :: FilePath -> Site -> IO ()
		writer folder_path = 
			let
				render :: DocSepProps -> Site -> String
				render config = Optic.down (CoreSerial.layer config)
				in render sep_props >>> Base.writeFile (folder_path </> file_name)
		reader :: FilePath -> IO (Either String Site)
		reader folder_path = 
			let
				parse :: DocSepProps -> String -> Either String Site
				parse config = 
					id
					>>> Optic.piso_interpret (CoreSerial.layer config) 
					>>> Bifunctor.first (Fana.show >>> Acc.extract)
				in map (parse sep_props) (Base.readFile (folder_path </> file_name))
		in Member "core content tree" writer reader

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
