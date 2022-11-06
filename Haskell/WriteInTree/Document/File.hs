module WriteInTree.Document.File
(
	Error, write'', read, read''
)
where

import Fana.Prelude
import Prelude (String, IO, FilePath)
import WriteInTree.Document.Core.Serial.Page.Main (Site)
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (Labels)
import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (..))
import WriteInTree.Document.Folder (FolderStructure (..))
import WriteInTree.Document.Main (Document (..))
import WriteInTree.Document.SepProps.Data (DocSepProps (..))

import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.Layers as CoreSerial
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos
import qualified WriteInTree.Document.Folder as Folder
import qualified WriteInTree.Document.SepProps.Simco as SepPropsSimco


type Text = Base.String

type LabeledPositioned = (Labels, Positioned Text)

render_sep_props :: DocSepProps -> String
render_sep_props = SepPropsSimco.to_simco_text

render_core'' :: DocSepProps -> Site -> String
render_core'' config = Optic.down (CoreSerial.layer config)

render_all'' :: Document -> FolderStructure String
render_all'' d =
	let
		config = docSepProps d
		config_text = render_sep_props config
		core_text = render_core'' config (docCore d)
		in FolderStructure config_text core_text

write'' :: FilePath -> Document -> IO ()
write'' address d = Folder.write address (render_all'' d)


data Error =
	  ErrorInCore (Pos.PositionedMb (Accu.Accumulated Text)) 
	| ErrorInSeparateProperties SepPropsSimco.ParseError

instance Fana.Showable Text Error where
	show = \case
		ErrorInCore details -> "error in reading core file: " <> Fana.show details
		ErrorInSeparateProperties details -> "error in separate properties file: " <> Fana.show details


parse_sep_props :: String -> Either Error DocSepProps
parse_sep_props = SepPropsSimco.parse_from_text >>> Bifunctor.first ErrorInSeparateProperties

parse_core'' :: DocSepProps -> String -> Either Error Site
parse_core'' config = Optic.piso_interpret (CoreSerial.layer config) >>> Bifunctor.first ErrorInCore

parse_core :: DocSepProps -> String -> Either Error Site
parse_core config = Optic.piso_interpret (CoreSerial.layer config) >>> Bifunctor.first ErrorInCore

parse_all'' :: FolderStructure String -> Either Error Document
parse_all'' fs = do
	config <- parse_sep_props (fs_separate_properties fs)
	core <- parse_core'' config (fs_tree fs)
	pure (Document config core)

parse_all :: FolderStructure String -> Either Error Document
parse_all fs = do
	config <- parse_sep_props (fs_separate_properties fs)
	core <- parse_core config (fs_tree fs)
	pure (Document config core)

read'' :: FilePath -> IO (Either Error Document)
read'' = Folder.read >>> map parse_all''

read :: FilePath -> IO (Either Error Document)
read = Folder.read >>> map parse_all
