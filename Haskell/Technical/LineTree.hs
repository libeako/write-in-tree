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
import Prelude ((<>))

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Prelude as Base

import qualified Fana.Data.Tree.ParseFromElemList as TreeParse
import qualified Fana.Develop.Test.Define as Test
import qualified Fana.Optic.Concrete.Categories.Iso as Optic
import qualified Fana.Serial.Bidir.Serializer as LR


type Char = Base.Char
type String = [Char]
type LangRule v = LR.Serializer () Char v

line_end_char :: Char
line_end_char = '\n'

lining :: Optic.Iso' String [String]
lining =
	let
		concat :: [String] -> String
		concat = map (: [[line_end_char]]) >>> Foldable.concat >>> Foldable.concat
	in
		Optic.Iso concat List.lines

type Line = (TreeParse.Hight, String)

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
coding_lines = lining >**> indenting


---------------------- tests ----------------------------

test_lining :: Test
test_lining = Test.single "lines" (Optic.test_iso_law lining ([[], ["hello"], ["hello", "world"]], []))

test_indenting :: Test
test_indenting = 
	Test.single "indentation"
		(Optic.test_iso_law indenting_a_line ([(0, ""), (1, "a")], []))

test :: Test
test = Test.bunch "line-tree" [test_lining, test_indenting]
