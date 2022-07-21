module WriteInTree.Document.Core.Serial.RichTextTree.Export
(
	Path.CommentElemD (..), Path.CommentElemDT,
	layer,
	Label.Elem (..),
)
where

import Data.Tree (Tree)
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Technical.TextTree.Data as Tt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label


layer :: Optic.Iso' (Tree Tt.Elem') (Tree Path.CommentElemDT)
layer = convert_from_describing_class_4 Path.layer >**> Path.comment_layer
