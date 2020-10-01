-- | implements the command (show default document configuration).
module WriteInTree.Document.SepProps.Command_ShowDefault
(
	doit,
)
where

import Prelude (IO, putStr)

import qualified Data.Default.Class as Default
import qualified WriteInTree.Document.SepProps.Simco as Simco


-- | executes the command.
doit :: IO ()
doit = putStr (Simco.to_simco_text Default.def)
