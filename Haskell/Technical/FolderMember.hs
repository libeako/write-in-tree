module Technical.FolderMember
(
	Member (..), lift_by_piso, member_string,
)
where

import Fana.Prelude
import Prelude (IO, String)
import System.FilePath (FilePath, (</>))

import qualified Data.Bifunctor as Bifunctor
import qualified Fana.Optic.Concrete.Prelude as Optic
import qualified Prelude as Base


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
