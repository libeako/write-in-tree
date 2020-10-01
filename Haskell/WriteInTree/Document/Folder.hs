module WriteInTree.Document.Folder
(
	FolderStructure(..),
	read,
)
where

import Prelude (IO, String)
import Data.Functor (void)
import System.FilePath (FilePath, (</>))
import Fana.Prelude
import qualified Prelude as Base


data FolderStructure e = FolderStructure
	{
		-- properties of the document stored separately from the tree
		fs_separate_properties :: e,
		-- the main data
		fs_tree :: e
	}
	deriving (Functor, Foldable, Traversable)

instance Applicative FolderStructure where
	pure v = FolderStructure v v
	f <*> x = FolderStructure 
		((fs_separate_properties f) (fs_separate_properties x))
		((fs_tree f) (fs_tree x)) 

-- the paths of the files relative to the folder
folder_internal_structure :: FolderStructure String
folder_internal_structure = FolderStructure "properties.simco.text" "tree.mm"

read :: FilePath -> IO (FolderStructure String)
read folder_path = traverse ((folder_path </>) >>> Base.readFile) folder_internal_structure

write :: FilePath -> FolderStructure String -> IO ()
write folder_path file_contents = 
	let
		write_file :: FilePath -> String -> IO ()
		write_file relative_path file_content = Base.writeFile (folder_path </> relative_path) file_content
	in (sequenceA >>> void) (liftA2 write_file folder_internal_structure  file_contents)

