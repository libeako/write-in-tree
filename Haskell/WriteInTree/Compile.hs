module WriteInTree.Compile 
(
	compile,
)
where


import Control.Monad.Except (ExceptT (..))
import Fana.Prelude
import Prelude (String, IO, FilePath)
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Main (Document (..))

import qualified Fana.Data.HeteroPair as HePair
import qualified Fana.Optic.Concrete.Categories.Interfaces as Optic
import qualified Fana.Optic.Concrete.Categories.Lens as Optic
import qualified WriteInTree.Document.File as File
import qualified WriteInTree.Document.Main as DocData
import qualified WriteInTree.Output.Technical as Ot
import qualified WriteInTree.Output.Xml.Render as Ott
import qualified Technical.Else as Tech


compile_website :: FilePath -> (SiteAddressMap, Site) -> Ot.Output
compile_website output_folder = 
	Ott.to_technical output_folder >>> HePair.after ""

compile :: Tech.FilePath {- output -} -> FilePath {- input -} -> ExceptT String IO ()
compile output_folder = 
	let 
		document_to_website :: (SiteAddressMap, Document) -> Ot.Output
		document_to_website = Optic.fn_up Optic.lens_2 DocData.docCore >>> compile_website (Tech.deFilePath output_folder) 
		in File.read >=> (document_to_website >>> Ot.write >>> map Right >>> ExceptT)
