module Example_fish where
import Prelude hiding (flip)
import Geometry (rot, rot45, flip, above, aboveS, beside, besideS, over, natrec)
import Image    (Image, scale, blank)
import Fish     (fish)

-- Just a test of directly building a fish, without web server, just for initial testing.

test_fish_square_limit :: Image
test_fish_square_limit =
  let fish2 = flip (rot45 fish) in
  let rot2  = \ p -> rot (rot p) in
  let rot3  = \ p -> rot (rot2 p) in
  let fish3 = rot3 fish2 in
  let t     = over fish (over fish2 fish3) in
  let u     = over  (over  fish2         (rot fish2))
                    (over  (rot2 fish2)  fish3) in
  let qrt   = \ p -> \ q -> \ r -> \ s -> above  (beside p q)
                                                 (beside r s) in
  let cyc   = \ p -> qrt p (rot p) (rot2 p) (rot3 p) in
  let side  = natrec blank (\ n -> \ img -> qrt img img (rot t) t)  in
  let corn  = natrec blank (\ n -> \ img -> qrt img (side n) (rot (side n)) u)  in
  let bes3  = \ a -> \ b -> \ c -> besideS 1 2 a (beside b c) in
  let abo3  = \ a -> \ b -> \ c -> aboveS  1 2 a (above  b c) in
  let nnet  = \ p -> \ q -> \ r ->
              \ s -> \ t -> \ u ->
              \ v -> \ w -> \ x -> abo3  (bes3 p q r)
                                         (bes3 s t u)
                                         (bes3 v w x) in
  let sqrl  = \ n -> nnet  (corn n)        (side n)         (rot3 (corn n))
                           (rot (side n))  u                (rot3 (side n))
                           (rot (corn n))  (rot2 (side n))  (rot2 (corn n)) in
  scale 1000 (sqrl 3)
  -- ends up as a list of 14008 splines

  -- scale 500 (bes3 fish fish2 fish3)
