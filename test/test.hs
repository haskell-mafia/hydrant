import           Disorder.Core.Main

import qualified Test.Hydrant as Hydrant

main :: IO ()
main =
  disorderMain [
      Hydrant.tests
    ]
