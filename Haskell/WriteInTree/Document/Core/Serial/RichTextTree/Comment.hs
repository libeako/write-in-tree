module WriteInTree.Document.Core.Serial.RichTextTree.Comment
(
	ElemD (..), ElemDT, ofElem_position,
	layer,
)
where

import Data.Default.Class
import Data.Tree (Tree)
import Fana.Prelude

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified Technical.TextTree.Data as Tt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String


-- | element type in the picture level.
type ElemP e = Path.DataElemO e
type ElemPT = ElemP Text

-- | meaningful [not comment] element type at the data level.
data ElemD e = ElemD 
	{ elemId :: Maybe Text
	, elemPosition :: Pos.Position
	, elemValue :: e
	}
	deriving (Eq, Functor, Foldable, Traversable)
type ElemDT = ElemD Text

ofElem_position :: Optic.Lens' Pos.Position (ElemD e)
ofElem_position = Optic.lens_from_get_set elemPosition (\ e c -> c { elemPosition = e })

instance Pos.HasPosition (ElemD e) where get_position = elemPosition
instance Default e => Default (ElemD e) where def = ElemD def def def


elem_pd :: ElemPT -> ElemDT
elem_pd (position, (Tt.Elem identifier text)) = ElemD
	{ elemId = identifier
	, elemPosition = position
	, elemValue = text 
	}
elem_dp :: ElemDT -> ElemPT
elem_dp e = (Pos.get_position e, Tt.Elem (elemId e) (elemValue e))

layer :: Optic.Iso' (Tree ElemPT) (Tree ElemDT)
layer = Optic.Iso (map elem_dp) (map elem_pd)
