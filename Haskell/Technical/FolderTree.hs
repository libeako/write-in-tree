module Technical.FolderTree
(
	Directory (..), read_directory_forest,
)
where

import Data.Maybe (catMaybes)
import Data.Tree (Tree, Forest)
import Fana.Prelude
import Prelude (IO)
import System.FilePath (FilePath, (</>))
import System.Directory (listDirectory, doesDirectoryExist)

import qualified Data.Tree as Tree


data Directory =
	Directory
	{ dirName :: FilePath
	, dirPath :: FilePath
	}

{-| . Output paths are relative as the input paths are. -}
read_directory_forest :: FilePath -> IO (Forest Directory)
read_directory_forest trunk =
	let
		read_child_at :: FilePath -> IO (Maybe (Forest Directory))
		read_child_at path =
			doesDirectoryExist path >>=
			\ case
				True -> map Just (read_child_dir path)
				False -> pure Nothing
		read_child_dir :: FilePath -> IO (Forest Directory)
		read_child_dir path = (read_directory_forest path)
		read_child :: FilePath -> IO (Maybe (Tree Directory))
		read_child entry =
			let
				path = trunk </> entry
				in (map >>> map) (Tree.Node (Directory entry path)) (read_child_at path)
		in map catMaybes ((listDirectory trunk) >>= traverse read_child)
