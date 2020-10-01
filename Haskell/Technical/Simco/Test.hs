module Technical.Simco.Test
(
	test,
)

where


import Fana.Develop.Test.Define (Test)

import qualified Fana.Develop.Test.Define as Test
import qualified Technical.Simco.Lines as Lines


test :: Test
test = Test.bunch "simco" [Lines.test]
