module Main where
import Control.Applicative
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Hock

main :: IO () 
main = defaultMain tests

instance Arbitrary Noun where
  arbitrary = oneof [ A <$> arbitrary
                    , C <$> arbitrary <*> arbitrary
                    ]

tests = [ testGroup "Nock rewrite rules" [ treeSelectionOp 
                                         , constOp
                                         ]
        ]

treeSelectionOp = testGroup "tree selection" [ wholeTreeSelection 
                                             , leftSubtreeSelection 
                                             , rightSubtreeSelection
                                             , deepSubtreeSelection
                                             ]

wholeTreeSelection = testProperty "whole tree selection" $ \tree -> 
  nock (C tree (C (A 0) (A 1))) == tree

leftSubtreeSelection = testProperty "left subtree selection" $ \(left, right) ->
  let tree = C left right
  in nock (C tree (C (A 0) (A 2))) == left

rightSubtreeSelection = testProperty "right subtree selection" $ \(left, right) ->
  let tree = C left right
  in nock (C tree (C (A 0) (A 3))) == right

deepSubtreeSelection = testProperty "right subtree selection" $ \deep ->
  let tree = C (C (A 4) deep) (A 3)
  in nock (C tree (C (A 0) (A 5))) == deep

constOp = testProperty "const operator" $ \(ignored, selected) ->
  nock (C ignored (C (A 1) selected)) == selected
