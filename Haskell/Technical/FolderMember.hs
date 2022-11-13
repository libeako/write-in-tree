module Technical.FolderMember
(
	Member (Member, memberName, memberWriter), 
	lift_by_piso, member_string, read,
	FileFormat (..), FileFormats, member_multi_format,
)
where

import Fana.Prelude
import Prelude (IO, String)
import System.FilePath (FilePath, (</>))

import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base
import qualified System.Directory as FileSys


data Member d =
	Member
	{ memberName :: String
	, memberWriter :: FilePath -> d -> IO ()
	, memberReader :: FilePath -> IO (Either String d)
	}

read :: FilePath -> Member d -> IO (d)
read folder_path (Member name _ reader) =
	let
		error_result message = Base.error ("could not read '" <> name <> "'\n" <> message)
		in map (either error_result id) (reader folder_path)

lift_by_piso :: forall l h . Optic.PartialIso' String l h -> Member l -> Member h
lift_by_piso (Optic.PartialIso render parse) (Member name writer reader) =
	let
		prefix_parse_error_message :: String -> String
		prefix_parse_error_message message = "while parsing " <> name <> ":" <> message
		prefixed_parse :: l -> Either String h
		prefixed_parse = parse >>> Bifunctor.first prefix_parse_error_message 
		in Member name (map (render >>>) writer) (map (map (>>= prefixed_parse)) reader)

member_string :: String -> FilePath -> Member String
member_string member_name file_name =
	let
		writer = (</> file_name) >>> Base.writeFile
		reader = (</> file_name) >>> Base.readFile
		in Member member_name writer (reader >>> map Right)

data FileFormat d =
	FileFormat
	{ ffFormatName :: String
	, ffFileName :: String
	, ffSerializer :: Optic.PartialIso' String String d
	}

type FileFormats d = (FileFormat d, [FileFormat d])

{-| returns None iff the file does not exist  -}
try_to_read_format :: forall d . FilePath -> FileFormat d -> IO (Maybe (Either String d))
try_to_read_format folder_path format =
	let
		file_path = folder_path </> ffFileName format
		parser = Optic.interpret (ffSerializer format)
		react_on_existence :: Bool -> IO (Maybe (Either String d))
		react_on_existence =
			\case
				False -> pure Nothing
				True -> map (parser >>> Just) (Base.readFile file_path)			
		in FileSys.doesFileExist file_path >>= react_on_existence

get_first_valid_monadic_just :: Monad m => [m (Maybe e)] -> m (Maybe e)
get_first_valid_monadic_just =
	\case
		[] -> pure Nothing
		(head : tail) -> head >>= maybe (get_first_valid_monadic_just tail) (Just >>> pure)

try_to_read_formats :: forall d . FilePath -> [FileFormat d] -> IO (Either String d)
try_to_read_formats folder_path = 
	map (try_to_read_format folder_path) >>> get_first_valid_monadic_just
	>>> map (maybe (Left "did not find it") id)
			
member_multi_format :: forall d . String -> FileFormats d -> Member d
member_multi_format member_name formats =
	let
		writer :: FilePath -> d -> IO ()
		writer folder_path =
			let
				format = fst formats 
				in Optic.down (ffSerializer format) >>> Base.writeFile (folder_path </> ffFileName format)
		reader :: FilePath -> IO (Either String d)
		reader = flip try_to_read_formats (fst formats : snd formats)
		in Member member_name writer reader
