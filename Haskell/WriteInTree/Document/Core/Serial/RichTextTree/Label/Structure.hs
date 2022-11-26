module WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure
(
	PageAddress (..),
	Any (..),
)
where

import Fana.Prelude

import qualified Prelude as Base


type Char = Base.Char
type Text = [Char]

data Any = IntermId Text

data PageAddress = 
	PageAddress { unwrapPageAddress :: Text }
	deriving Eq
