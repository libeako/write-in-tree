module Technical.Simco.DataRender
(
	render_tree, render_forest,
)
where

import Fana.Prelude
import Data.Tree (Tree (..))
import Prelude (String)

import qualified Data.Foldable as Foldable
import qualified Technical.Simco.Data as High -- high level data
import qualified Technical.Simco.DataLines as Low -- low level data


type Text = String

meaningful_common_render :: High.MeaningfulCommon -> Low.MeaningfulCommon
meaningful_common_render (High.MeaningfulCommon is_active name) = Low.MeaningfulCommon is_active name

render_tree :: High.Node -> [Tree Low.Node]
render_tree =
	\case
		High.NodeMeaningful mn -> 
			case mn of
				High.MnProperty meaningful_common value children -> 
					let node_data = Low.NodeMeaningful (Low.NodeProperty (meaningful_common_render meaningful_common) value)
					in [Node node_data ((map <<< map) Low.NodeComment children)]
				High.MnCategory meaningful_common children ->
					let trunk = Low.NodeMeaningful (Low.NodeCategory (meaningful_common_render meaningful_common))
					in [Node trunk (render_forest children)]
		High.NodeComment comment_tree -> []

render_forest :: [High.Node] -> [Tree Low.Node]
render_forest = map render_tree >>> Foldable.concat



