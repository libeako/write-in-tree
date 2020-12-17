module Technical.LineTree
(
	Line, 
	coding_lines,
	test,
)
where

import Fana.Prelude
import Fana.Develop.Test.Define (Test)
import Fana.Math.Algebra.Category.OnTypePairs ((>**>))

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Prelude as Base

import qualified Fana.Data.Tree.SerializeHight as TreeSerial
import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Optic.Concrete.Categories.Iso as Optic
import qualified Fana.Serial.Bidir.Instances.Text.Lines as Serial
import qualified Fana.Serial.Bidir.Serializer as LR


type Char = Base.Char
type String = [Char]
type LangRule v = LR.Serializer () Char v

type Line = (TreeSerial.Hight, String)

indent_char :: Char
indent_char = '\t'

indenting_a_line :: Optic.Iso' String Line
indenting_a_line = 
	let
		render :: Line -> String
		render (count, content) = List.replicate count indent_char <> content
		parse :: String -> Line
		parse = List.span (== indent_char) >>> Bifunctor.first List.length >>> uncurry (,)
	in
		Optic.Iso render parse

indenting :: Optic.Iso' [String] [Line]
indenting = Optic.iso_up indenting_a_line

coding_lines :: Optic.Iso' String [Line]
coding_lines = Serial.lines >**> indenting


---------------------- tests ----------------------------

test_indenting :: Test
test_indenting = 
	Test.single "indentation"
		(Optic.test_iso_law indenting_a_line ([(0, ""), (1, "a")], []))

test :: Test
test = Test.bunch "line-tree" [test_indenting]
