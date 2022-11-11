module WriteInTree.Document.File
(
	write, read,
)
where

import Fana.Prelude
import Prelude (String, IO, FilePath)
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
import qualified WriteInTree.Document.Folder as Folder
import qualified WriteInTree.Document.SepProps.File as SepProps
import qualified WriteInTree.Document.SepProps.Simco as SepPropsSimco


type Text = Base.String


sep_props_file_name :: String
sep_props_file_name = "properties.simco.text"


render_sep_props :: DocSepProps -> String
render_sep_props = SepPropsSimco.to_simco_text

render_core :: DocSepProps -> Site -> String
render_core config = Optic.down (CoreSerial.layer config)

render_all :: Document -> String
render_all d =
	let
		config = docSepProps d
		in render_core config (docCore d)

write :: FilePath -> Document -> IO ()
write address d = 
	do
		Directory.createDirectory address
		SepProps.write address (docSepProps d)
		Folder.write address (render_all d)

parse_core :: DocSepProps -> String -> Either String Site
parse_core config = Optic.piso_interpret (CoreSerial.layer config) >>> Bifunctor.first (Fana.show >>> Acc.extract)

read :: FilePath -> Monad.ExceptT Text IO Document
read folder_path = 
	do
		config <- SepProps.read folder_path
		core_file <- Monad.ExceptT (map Right (Folder.read folder_path))
		core <- 
			let
				raw_result = parse_core config core_file
				in Monad.ExceptT (pure raw_result)
		pure (Document config core)
