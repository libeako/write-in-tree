module WriteInTree.Document.Core.Serial.RichTextTree.Comment
(
	ElemD (..), ElemDT, ofElem_position,
	ParseError (..),
	layer,
)
where

import Data.Default.Class
import Data.Tree (Tree)
import Fana.Prelude

import qualified Data.Tree as Base
import qualified Data.Tree as Tree
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
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
	, elemPosition :: Pos.PositionFields
	, elemValue :: e
	}
	deriving (Eq, Functor, Foldable, Traversable)
type ElemDT = ElemD Text

ofElem_position :: Optic.Lens' Pos.PositionFields (ElemD e)
ofElem_position = Optic.lens_from_get_set elemPosition (\ e c -> c { elemPosition = e })

instance Pos.HasPosition (ElemD e) where get_position = elemPosition >>> Pos.field_source_path
instance Default e => Default (ElemD e) where def = ElemD def def def


elem_pd :: ElemPT -> ElemDT
elem_pd (position, (Tt.Elem identifier text)) = ElemD
	{ elemId = identifier
	, elemPosition = Pos.PositionFields position
	, elemValue = text 
	}
elem_dp :: ElemDT -> ElemPT
elem_dp e = (Pos.get_position e, Tt.Elem (elemId e) (elemValue e))

meta_name_comment :: Text
meta_name_comment = "comment"

type ParseError = Pos.PositionedMb (Accu.Accumulated Text)

type Errorable e = Either ParseError e

parse :: Tree ElemPT -> Errorable (Tree ElemDT)
parse (Base.Node trunk children) = map (Base.Node (elem_pd trunk)) (traverse parse children)

render :: Tree ElemDT -> Tree ElemPT
render (Tree.Node elem children) = Tree.Node (elem_dp elem) (map render children)

layer :: Optic.PartialIso' ParseError (Tree ElemPT) (Tree ElemDT)
layer = Optic.PartialIso render parse

