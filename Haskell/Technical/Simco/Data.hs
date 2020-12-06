module Technical.Simco.Data
(
	MeaningfulCommon (..), MeaningfulNode (..), Node (..),
	comment, property, category,
)
where

import Fana.Prelude
import Prelude (String)
import Data.Tree (Tree(..))


type Text = String

data MeaningfulCommon = MeaningfulCommon { mcIsActive :: Bool, mcName :: Text }
data MeaningfulNode = MnCategory MeaningfulCommon [Node] | MnProperty MeaningfulCommon Text [Tree Text]
data Node = NodeMeaningful MeaningfulNode | NodeComment (Tree Text)


-- * helper constructors

-- | just a different name for the regular 'Node' constructor for a simple comment, 
-- one that is consistent with other helper constructors.
comment :: Text -> Node
comment = flip Node [] >>> NodeComment

property :: Text -> Text -> [Tree Text] -> Node
property name value comments = NodeMeaningful (MnProperty (MeaningfulCommon True name) value comments)

category :: Text -> [Node] -> Node
category name content = NodeMeaningful (MnCategory (MeaningfulCommon True name) content)
