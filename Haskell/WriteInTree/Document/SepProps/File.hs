module WriteInTree.Document.SepProps.File
(
	 write, read,
)
where

import Fana.Prelude
import Prelude (String, IO)
import WriteInTree.Document.SepProps.Data (DocSepProps (..))
import System.FilePath ((</>))

import qualified Control.Monad.Except as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Math.Algebra.Monoid.Accumulate as Acc
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified WriteInTree.Document.SepProps.Simco as SepPropsSimco


type Text = Base.String
type Filepath = Text


file_name :: String
file_name = "properties.simco.text"

full_file_path :: Filepath -> String
full_file_path = (</> file_name)

render :: DocSepProps -> String
render = SepPropsSimco.to_simco_text

write :: Filepath -> DocSepProps -> IO ()
write folder_path = render >>> Base.writeFile (full_file_path folder_path) 

read :: Filepath -> Monad.ExceptT Text IO DocSepProps
read =
	let
		parse :: String -> Either String DocSepProps
		parse = 
			id
			>>> SepPropsSimco.parse_from_text
			>>> Bifunctor.first (Fana.show >>> Acc.extract >>> ("error in separate properties file:\n" <>))
		in
			id
			>>> full_file_path
			>>> Base.readFile
			>>> map parse
			>>> Monad.ExceptT
