module WriteInTree.Compile 
(
	compile,
)
where


import Control.Monad.Except (ExceptT (..))
import Fana.Prelude
import Prelude (String, IO, FilePath)
import WriteInTree.Document.Core.Data (Site)
import WriteInTree.Document.Main (Document (..))

import qualified Fana.Data.HeteroPair as HePair
import qualified WriteInTree.Document.File as File
import qualified WriteInTree.Document.Main as DocData
import qualified WriteInTree.Output.Technical as Ot
import qualified WriteInTree.Output.Xml.Render as Ott
import qualified Technical.Else as Tech


compile_website :: FilePath -> Site -> Ot.Output
compile_website output_folder = 
	Ott.to_technical output_folder >>> HePair.after ""

compile :: Tech.FilePath {- output -} -> FilePath {- input -} -> ExceptT String IO ()
compile output_folder = 
	let 
		document_to_website :: Document -> Ot.Output
		document_to_website = DocData.docCore >>> compile_website (Tech.deFilePath output_folder) 
		in File.read >=> (document_to_website >>> Ot.write >>> map Right >>> ExceptT)
