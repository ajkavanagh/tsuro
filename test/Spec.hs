import Test.Hspec        (Spec)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)


import TileSpec

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = tileSpecs

