module WriteInTree.Document.Core.Serial.RichTextTree.Label.Labeled
(
	Structure.add_new_classes_to_Labels,
	Labeled (..),
)
where

import WriteInTree.Document.Core.Serial.RichTextTree.Position (Positioned (..))
import WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure (Labels)

import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure as Structure


type Char = Base.Char
type Text = [Char]


type Labeled e = (Labels, Positioned e)

