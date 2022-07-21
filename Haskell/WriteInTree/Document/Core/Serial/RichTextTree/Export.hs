module WriteInTree.Document.Core.Serial.RichTextTree.Export
(
	Comment.ElemD (..), Comment.ElemDT,
	layer,
	Label.Elem (..),
)
where

import Data.Tree (Tree)
import Fana.Haskell.DescribingClass
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))

import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Technical.TextTree.Data as Tt
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Comment as Comment
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Path as Path
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label


layer :: Optic.Iso' (Tree Tt.Elem') (Tree Comment.ElemDT)
layer = convert_from_describing_class_4 Path.layer >**> Comment.layer
