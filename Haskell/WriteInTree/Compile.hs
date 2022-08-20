module WriteInTree.Compile 
(
	compile,
)
where


import Control.Monad ((>=>))
import Fana.Prelude
import Prelude (IO, FilePath)

import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Data.HeteroPair as HePair
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as Data
import qualified WriteInTree.Document.Core.ToOutput as UsiToO
import qualified WriteInTree.Document.Data as DocData
import qualified WriteInTree.Document.File as DocRead
import qualified WriteInTree.Output.Pagination as Page
import qualified WriteInTree.Output.Technical as Ot
import qualified WriteInTree.Output.Xml.Render as Ott
import qualified Technical.Else as Tech


type Text = Base.String


process_website_compilation_result :: Either DocRead.Error Ot.FileOps -> Ot.Output
process_website_compilation_result = 
	let error_message e = Accu.extract ("Compile error: " <> Fana.show e)
	in Bifunctor.bimap error_message (HePair.after "")

compile_website :: Bool -> FilePath -> Either DocRead.Error (Page.Site Data.NodeIdU) -> Ot.Output
compile_website sentencing output_folder = 
	map (Ott.to_technical sentencing output_folder) >>> process_website_compilation_result

compile :: Bool -> Tech.FilePath {- output -} -> FilePath {- input -} -> IO ()
compile sentencing output_folder = 
	let 
		translate :: DocData.Data () Data.NodeIdU Data.NodeIdU -> Page.Site Data.NodeIdU
		translate = DocData.doc_core >>> UsiToO.translate
		document_to_website = map translate >>> compile_website sentencing (Tech.deFilePath output_folder) 
	in DocRead.read >=> (document_to_website >>> Ot.write)
