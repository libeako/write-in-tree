module WriteInTree.Compile 
(
	compile,
)
where


import Control.Monad ((>=>))
import Fana.Prelude
import Prelude (IO, FilePath, String)
import WriteInTree.Document.Core.Serial.Page.Main (Site)
import WriteInTree.Document.Main (Document (..))

import qualified Control.Monad.Except as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Data.HeteroPair as HePair
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Serial.Print.Show as Fana
import qualified WriteInTree.Document.File as DocRead
import qualified WriteInTree.Document.Main as DocData
import qualified WriteInTree.Output.Technical as Ot
import qualified WriteInTree.Output.Xml.Render as Ott
import qualified Technical.Else as Tech


process_website_compilation_result :: Either String Ot.FileOps -> Ot.Output
process_website_compilation_result = 
	let error_message e = Accu.extract ("Compile error: " <> Fana.show e)
		in Bifunctor.bimap error_message (HePair.after "")

compile_website :: Bool -> FilePath -> Either String Site -> Ot.Output
compile_website sentencing output_folder = 
	map (Ott.to_technical sentencing output_folder) >>> process_website_compilation_result

compile :: Bool -> Tech.FilePath {- output -} -> FilePath {- input -} -> IO ()
compile sentencing output_folder = 
	let 
		document_to_website :: Either String Document -> Ot.Output
		document_to_website = map DocData.docCore >>> compile_website sentencing (Tech.deFilePath output_folder) 
		in (DocRead.read >>> Monad.runExceptT) >=> (document_to_website >>> Ot.write)
