module WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure
(
	PageAddress (..),
	Any (..),
	Labels (..), no_Labels,
)
where

import Data.Default.Class
import Fana.Prelude

import qualified Prelude as Base


type Char = Base.Char
type Text = [Char]

data Any = IntermId Text

data PageAddress = 
	PageAddress { unwrapPageAddress :: Text }
	deriving Eq

-- | user given labels of a node.
data Labels = Labels
	{
	}
	deriving (Eq)

no_Labels :: Labels
no_Labels = Labels

instance Default Labels where def = no_Labels
