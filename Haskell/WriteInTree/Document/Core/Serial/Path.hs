module WriteInTree.Document.Core.Serial.Path
(
	layer,
)
where

import Data.Tree (Forest)
import Fana.Prelude
import WriteInTree.Document.Core.Data
import WriteInTree.Document.Core.Serial.Position

import qualified Fana.Optic.Concrete.Prelude as Optic


layer :: Optic.Iso (Forest Text) (Forest (Count, Text)) (Forest Text) (Forest (Positioned Text))
layer = Optic.Iso id (map (map (uncurry Positioned)))
