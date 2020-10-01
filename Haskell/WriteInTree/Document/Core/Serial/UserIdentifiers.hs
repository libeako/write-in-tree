module WriteInTree.Document.Core.Serial.UserIdentifiers
(
	L, H, L', H',
	up, layer,
)
where

import Data.Monoid (Monoid (..))
import Data.Tree (Tree (..))
import Fana.Data.Identified (Identified (Identified))
import Fana.Prelude
import Prelude ((+), Char, String)

import qualified Control.Monad.State.Strict as Mtl
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Base
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Maybe as Base
import qualified Data.Tree as Tree
import qualified Fana.Data.Identified as Identified
import qualified Fana.Data.Key.LensToMaybeElement as LensAt
import qualified Fana.Data.Key.Map.Interface as MapI
import qualified Fana.Data.Key.Map.KeyIsString as MapS
import qualified Fana.Data.Key.Traversable as Trav
import qualified Fana.Math.Algebra.Monoid.Accumulate as Accu
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified WriteInTree.Document.Core.Data as UsI
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Label.Main as Label
import qualified WriteInTree.Document.Core.Serial.RichTextTree.Position as Pos


type Text = [Char]
type A = Label.Elem Text
type InputElem a = UsI.Node (a ()) (a ()) Text Text Text
type InputElem' = InputElem A
type InputTree = Tree InputElem'

type ReferenceL = Text
type ReferenceH = UsI.NodeIdU
type StructureAsTree al a e r = UsI.StructureAsTree al a r r e
type L al a e = StructureAsTree al a e ReferenceL
type H al a e = StructureAsTree al a e ReferenceH
type L' a = L (a ()) (a ()) Text
type H' a = H (a ()) (a ()) Text



make_key_value_pair :: UsI.NodeIdU -> (Text, UsI.NodeIdU)
make_key_value_pair = liftA2 (,) (Identified.cargo >>> UsI.nidun_u) id

type Count = Base.Int

count :: UsI.NodeIdUCore -> Mtl.State Count UsI.NodeIdU
count core = do
	Mtl.modify' (+1)
	current_count <- Mtl.get
	pure (Identified current_count core)

make_changed_NodeIdUCore ::  UsI.Node al a id_u Text e -> Text -> UsI.NodeIdUCore
make_changed_NodeIdUCore n idu =
	UsI.NodeIdUCore (UsI.nodeIdAuto n) idu 
		((Pos.get_position >>> map Pos.text) (UsI.nodeWitSource n))

node_richener :: UsI.Node al a Text Text e -> UsI.Node al a UsI.NodeIdUCore Text e
node_richener n = Optic.fn_up UsI.idu_in_Node (make_changed_NodeIdUCore n) n

node_idu_richener' ::
	forall a e al. 
	UsI.StructureAsTree al a Text Text e ->
	UsI.StructureAsTree al a UsI.NodeIdUCore Text e
node_idu_richener' = Optic.fn_up UsI.node_in_tree node_richener

node_idu_richener ::
	forall a e al. 
	UsI.StructureAsTree al a Text Text e ->
	UsI.StructureAsTree al a UsI.NodeIdU Text e
node_idu_richener = node_idu_richener' >>> Optic.traverse UsI.idu_in_tree count >>> flip Mtl.evalState 0

gather_map :: UsI.StructureAsTree al a UsI.NodeIdU Text e -> MapS.Map Char [UsI.NodeIdU]
gather_map = id
	>>> Fold.toList
	>>> map UsI.uid_of_node >>> Base.catMaybes
	>>> map make_key_value_pair
	>>> MapI.from_list_lists

error_location_as_text :: [String] -> Accu.Accumulated Text
error_location_as_text path = Base.fold (map Accu.single (List.intersperse " -> " path))

same_id_error_message :: (Text, [UsI.NodeIdU]) -> Accu.Accumulated Text
same_id_error_message = let
	instance_in_line :: [String] -> Accu.Accumulated Text
	instance_in_line n = Accu.single "\n* " <> error_location_as_text n
	message_text :: Text -> [[Text]] -> Accu.Accumulated Text
	message_text identifier paths = mempty
		<> Base.foldMap Accu.single ["These nodes have the same identifier [\"", identifier, "\"] :"]
		<> Base.fold (map instance_in_line paths)
	in map (map (Identified.cargo >>> UsI.nidun_path_to_trunk)) >>> uncurry message_text

type Map = MapS.Map Char UsI.NodeIdU

singlify_map :: MapS.Map Char [UsI.NodeIdU] -> Either (Accu.Accumulated Text) Map
singlify_map = Trav.singlify_element_lists >>> Bifunctor.first same_id_error_message

create_map :: UsI.StructureAsTree al a UsI.NodeIdU Text e -> Either (Accu.Accumulated Text) Map
create_map = gather_map >>> singlify_map

invlid_id_error_message :: Text -> Accu.Accumulated Text
invlid_id_error_message identifier = Base.foldMap Accu.single
	["the identifier \"", identifier, "\" is of none of the nodes"]

change_identifier :: Map -> Text -> Either (Accu.Accumulated Text) UsI.NodeIdU
change_identifier m identifier = 
	Base.maybe (Left (invlid_id_error_message identifier)) Right (LensAt.get_at identifier m)

change_reference_from_node :: 
	forall al a e .
	Map -> 
	UsI.Node al a UsI.NodeIdU Text e -> 
	Either (Pos.Positioned (Accu.Accumulated Text)) (UsI.Node al a UsI.NodeIdU UsI.NodeIdU e)
change_reference_from_node m node = let
	computation_result :: Either (Accu.Accumulated Text) (UsI.Node al a UsI.NodeIdU UsI.NodeIdU e)
	computation_result = Optic.traverse UsI.internal_address_in_node (change_identifier m) node
	convert_error :: Accu.Accumulated Text -> Pos.Positioned (Accu.Accumulated Text)
	convert_error = Pos.Positioned (Pos.get_position (UsI.nodeWitSource node))
	in Bifunctor.first convert_error computation_result

change_references_in_tree ::
	Map ->
	UsI.StructureAsTree al a UsI.NodeIdU Text e -> 
	Either (Pos.Positioned (Accu.Accumulated Text)) (UsI.StructureAsTree al a UsI.NodeIdU UsI.NodeIdU e)
change_references_in_tree m = Optic.traverse UsI.node_in_tree (change_reference_from_node m)


change_references ::
	UsI.StructureAsTree al a UsI.NodeIdU Text e -> 
	Either (Pos.Positioned (Accu.Accumulated Text)) (UsI.StructureAsTree al a UsI.NodeIdU UsI.NodeIdU e)
change_references input = let
	convert_error :: Accu.Accumulated Text -> Pos.Positioned (Accu.Accumulated Text)
	convert_error = Pos.Positioned (Pos.get_position (UsI.nodeWitSource (Tree.rootLabel input)))
	in 
		Bifunctor.first convert_error (create_map input) 
		>>= flip change_references_in_tree input

up ::
	UsI.StructureAsTree al a Text Text e -> 
	Either (Pos.Positioned (Accu.Accumulated Text)) (UsI.StructureAsTree al a UsI.NodeIdU UsI.NodeIdU e)
up = node_idu_richener >>> change_references

reference_down :: UsI.NodeIdU -> Text
reference_down = Identified.cargo >>> UsI.nidun_u

down ::
	UsI.StructureAsTree al a UsI.NodeIdU UsI.NodeIdU e ->
	UsI.StructureAsTree al a Text Text e
down = id 
	>>> Optic.fn_up UsI.idu_in_tree reference_down
	>>> Optic.fn_up UsI.internal_address_in_tree reference_down

layer :: Optic.PartialIso' (Pos.Positioned (Accu.Accumulated Text)) 
	(UsI.StructureAsTree al a Text Text e) 
	(UsI.StructureAsTree al a UsI.NodeIdU UsI.NodeIdU e)
layer = Optic.PartialIso down up
