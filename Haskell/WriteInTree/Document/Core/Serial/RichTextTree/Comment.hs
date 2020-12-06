module WriteInTree.Document.Core.Serial.RichTextTree.Comment
(
	ElemD (..), ElemDT, ofElem_position,
	CommentElem,
	ParseError (..),
	layer,
)
where

import Control.Monad ((>=>))
import Data.Default.Class
import Data.Tree (Tree)
import Fana.Prelude

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Either as Base
import qualified Data.Tree as Base
import qualified Data.Tree as Tree
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Fana.Serial.Print.Show as Fana
import qualified Prelude as Base
import qualified Technical.TextTree.Data as Tt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.InNode.TextStructure as Ts
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Ord as Ord
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = Base.String


-- | element type in the picture level.
type ElemP e = Path.DataElemO e
type ElemPT = ElemP Text

type Ordered e = Ord.Ordered (Tt.Elem e)
type CommentElem = ElemP [Base.Tree ElemPT]

map_Ordered :: (x -> y) -> (Ordered x -> Ordered y)
map_Ordered = map >>> map

map_ElemP :: (x -> y) -> (ElemP x -> ElemP y)
map_ElemP = map_Ordered >>> map

-- | meaningful [not comment] element type at the data level.
data ElemD e = ElemD 
	{ elemId :: Maybe Text
	, elemPosition :: Pos.PositionFields
	, elemCommentChildren :: [CommentElem]
	, elemValue :: e
	}
	deriving (Functor, Foldable, Traversable)
type ElemDT = ElemD Text

ofElem_position :: Optic.Lens' Pos.PositionFields (ElemD e)
ofElem_position = Optic.lens_from_get_set elemPosition (\ e c -> c { elemPosition = e })

instance Pos.HasPosition (ElemD e) where get_position = elemPosition >>> Pos.field_source_path
instance Default e => Default (ElemD e) where def = ElemD def def def def


elem_pd :: [CommentElem] -> ElemPT -> ElemDT
elem_pd comment_children (position, (ordinal, (Tt.Elem identifier text))) = ElemD
	{ elemId = identifier
	, elemPosition = Pos.PositionFields ordinal position
	, elemCommentChildren = comment_children
	, elemValue = text 
	}
elem_dp :: ElemDT -> ElemPT
elem_dp e = (Pos.get_position e, (Pos.field_ordinal (elemPosition e), Tt.Elem (elemId e) (elemValue e)))	

meta_name_comment :: Text
meta_name_comment = "comment"

-- ~ data ParseError 
	-- ~ = TextStructureErrorInElement (Path.Positioned Ts.TextStructureError)
	-- ~ | WholeTreeIsComment
	-- ~ deriving Show

newtype IsComment = IsComment { is_comment :: Bool }

type ParseError = Pos.PositionedMb (Accu.Accumulated Text)

type Errorable e = Either ParseError e

elem_is_comment :: ElemPT -> Errorable IsComment
elem_is_comment (position, (_, (Tt.Elem _ text))) = 
	let
		text_is_meta_comment :: Ts.Content Text Text -> IsComment
		text_is_meta_comment = Base.either (== meta_name_comment) (const False) >>> IsComment
		in Bifunctor.bimap (Fana.show >>> Pos.PositionedMb (Just position)) text_is_meta_comment (Ts.parse text)

parse_subtree :: Tree ElemPT -> Errorable (Either CommentElem (Tree ElemDT))
parse_subtree (Base.Node trunk children) = 
	let
		branch_on_commentness :: IsComment -> Errorable (Either CommentElem (Tree ElemDT))
		branch_on_commentness (IsComment True) = 
			let 
				comment_elem :: CommentElem
				comment_elem = map_ElemP (const children) trunk
				in pure (Left comment_elem)
		branch_on_commentness (IsComment False) = 
			do
				parsed_children :: [Either CommentElem (Tree ElemDT)] <- traverse parse_subtree children
				let (comment_children, meaningful_children) = 
					Base.partitionEithers parsed_children :: ([CommentElem], [Tree ElemDT])
				let new_node = elem_pd comment_children trunk :: ElemDT
				let new_tree = (Base.Node new_node meaningful_children :: Tree ElemDT)
				pure (Right new_tree)
		in elem_is_comment trunk >>= branch_on_commentness

parse :: Tree ElemPT -> Errorable (Tree ElemDT)
parse = let
	error_message_whole_tree_comment = 
		Pos.without_position "the whole tree is a comment, which is not valid"
	in parse_subtree >=> Base.either (const (Left error_message_whole_tree_comment)) Right

children_from_CommentElem :: CommentElem -> [Base.Tree ElemPT]
children_from_CommentElem = snd >>> snd >>> Tt.elemValue

elem_from_CommentElem :: CommentElem -> ElemPT
elem_from_CommentElem = map_ElemP (const (Ts.render (Left meta_name_comment)))

render_CommentElem :: CommentElem -> Tree ElemPT
render_CommentElem = liftA2 Base.Node elem_from_CommentElem children_from_CommentElem

render_MeaningfulElem :: Tree ElemDT -> Tree ElemPT
render_MeaningfulElem (Tree.Node elem children) = 
	let
		comment_children :: [Tree ElemPT]
		comment_children = map render_CommentElem (elemCommentChildren elem)
		meaningful_children :: [Tree ElemPT]
		meaningful_children = map render_MeaningfulElem children
		ordered_children = comment_children <> meaningful_children
		in Tree.Node (elem_dp elem) ordered_children


layer :: Optic.PartialIso' ParseError (Tree ElemPT) (Tree ElemDT)
layer = Optic.PartialIso render_MeaningfulElem parse

