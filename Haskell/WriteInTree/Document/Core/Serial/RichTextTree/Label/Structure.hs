module WriteInTree.Document.Core.Serial.RichTextTree.Label.Structure
(
	PageAddress (..),
	Any (..),
	Labels (..), no_Labels, inLabel_page_address,
)
where

import Data.Default.Class
import Fana.Prelude

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


type Char = Base.Char
type Text = [Char]

data Any = IntermId Text

data PageAddress = 
	PageAddress { unwrapPageAddress :: Text }
	deriving Eq

-- | user given labels of a node.
data Labels = Labels
	{ address_of_Labels :: Maybe PageAddress
	}
	deriving (Eq)

no_Labels :: Labels
no_Labels = Labels Nothing

instance Default Labels where def = no_Labels

inLabel_page_address :: Optic.Lens (Maybe PageAddress) (Maybe PageAddress) Labels Labels
inLabel_page_address = Optic.lens_from_get_set address_of_Labels (\ p w -> w { address_of_Labels = p })
