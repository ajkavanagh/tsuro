module TileInternalSpec where

import           Test.Hspec (Spec, describe, it, pending, shouldBe, xit)

import           Data.List  (sort)
import           Data.Set   (fromList)

import           Tile.Internal

tileSpecs :: Spec
tileSpecs = do

    describe "there should be 35 unique tiles with number 0 - 7" $ do

        it "number of tiles should be 35" $
            length uniqueTiles `shouldBe` 35

        it "All the tiles should be unique" $
            length (fromList uniqueTiles) `shouldBe` 35

        it "All tiles should be 8 numbers, 0..7" $
            all consistentTile uniqueTiles `shouldBe` True

    describe "tiles can be rotated" $ do

        it "Rotating 01-23-45-67 should be the same" $
            rotateTile [0..7] `shouldBe` [0..7]

        it "Rotated tile set for 01-23-45-67 should be the same" $
            rotatedTilesSet [0..7] `shouldBe` [[0..7]]

        it "Rotated tile set for 01-23-46-57 should be 4 long and match given" $
            rotatedTilesSet [0,1,2,3,4,6,5,7] `shouldBe`
                [ [0,1,2,3,4,6,5,7]
                , [0,1,2,4,3,5,6,7]
                , [0,2,1,3,4,5,6,7]
                , [0,6,1,7,2,3,4,5]
                ]

        it "rotating a tile keeps it consistent" $
            all consistentTile $ concatMap rotatedTilesSet uniqueTiles

        it "rotated tiles should be unique and not in uniqueTiles" $ do
            let rotated = concatMap (tail.rotatedTilesSet) uniqueTiles
            all (`notElem` uniqueTiles) rotated && not (null rotated)



consistentTile :: Integral a => [a] -> Bool
consistentTile xs = sort xs == [0..7]
