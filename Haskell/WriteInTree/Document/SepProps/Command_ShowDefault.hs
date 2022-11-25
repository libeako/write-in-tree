-- | implements the command (show default document configuration).
module WriteInTree.Document.SepProps.Command_ShowDefault
(
	doit,
)
where

import Prelude (IO, putStr)

import qualified Data.Default.Class as Default
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified WriteInTree.Document.SepProps.PropTree as PT
import qualified WriteInTree.Document.SepProps.Simco as Simco

-- | executes the command.
doit :: IO ()
doit = putStr (Optic.down (Simco.layer PT.type_structure_doc_sep_props) Default.def)
