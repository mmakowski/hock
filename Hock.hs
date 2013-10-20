module Hock where

data Noun = A Int
          | C Noun Noun
  deriving (Eq, Show)

yes = A 0
no  = A 1

isCell :: Noun -> Noun
isCell (C _ _) = yes
isCell _       = no

inc :: Noun -> Noun
inc (A n) = A (n+1)
inc c     = error $ "can't increment a cell " ++ (show c)

isEq :: Noun -> Noun
isEq (C a b) = if a == b then yes else no
isEq a       = error $ "can't compare an atom " ++ (show a)

subtree :: Noun -> Noun -> Noun
subtree (A 1) a       = a
subtree (A 2) (C a b) = a
subtree (A 3) (C a b) = b
subtree (A n) b@(C _ _)  
  | n `mod` 2 == 0    = subtree (A 2) (subtree (A (n `div` 2)) b)
  | otherwise         = subtree (A 3) (subtree (A ((n-1) `div` 2)) b)
subtree (A _) (A _)   = error $ "unable to take a non-1 subtree of an atom"
subtree c     _       = error $ "invoked subtree with non-atomic first argument: " ++ (show c)

nock :: Noun -> Noun
nock (C a (C (C b c)  d))       = C (nock (C a (C b c))) (nock (C a d))
nock (C a (C (A  0)   b))       = subtree b a
nock (C _ (C (A  1)   b))       = b
nock (C a (C (A  2)   (C b c))) = nock $ C (nock (C a b)) (nock (C a c))
nock (C a (C (A  3)   b))       = isCell $ nock (C a b)
nock (C a (C (A  4)   b))       = inc $ nock (C a b)
nock (C a (C (A  5)   b))       = isEq $ nock (C a b)
nock (C a (C (A  6)   (C b (C c d)))) = 
  nock (C a (C (A 2) (C (C yes no) (C (A 2) (C (C no (C c d)) (C (C no yes) (C (A 2) (C (C (A 1) (C (A 2) (A 3))) (C (C no yes) (C (A 4) (C (A 4) b)))))))))))
nock (C a (C (A  7)   (C b c))) = nock (C a (C (A 2) (C b (C (A 1) c))))
nock (A n)                     = error $ "*" ++ (show n)
