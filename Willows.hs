module Willows where

import qualified Prelude

data Nat =
   O
 | S Nat

app :: (([]) a1) -> (([]) a1) -> ([]) a1
app l m =
  case l of {
   ([]) -> m;
   (:) a l1 -> (:) a (app l1 m)}

add :: Nat -> Nat -> Nat
add n m =
  case n of {
   O -> m;
   S p -> S (add p m)}

add0 :: Nat -> Nat -> Nat
add0 n m =
  case n of {
   O -> m;
   S p -> S (add0 p m)}

sub :: Nat -> Nat -> Nat
sub n m =
  case n of {
   O -> n;
   S k -> case m of {
           O -> n;
           S l -> sub k l}}

min :: Nat -> Nat -> Nat
min n m =
  case n of {
   O -> O;
   S n' -> case m of {
            O -> O;
            S m' -> S (min n' m')}}

data Positive =
   XI Positive
 | XO Positive
 | XH

data Z =
   Z0
 | Zpos Positive
 | Zneg Positive

succ :: Positive -> Positive
succ x =
  case x of {
   XI p -> XO (succ p);
   XO p -> XI p;
   XH -> XO XH}

add1 :: Positive -> Positive -> Positive
add1 x y =
  case x of {
   XI p ->
    case y of {
     XI q -> XO (add_carry p q);
     XO q -> XI (add1 p q);
     XH -> XO (succ p)};
   XO p ->
    case y of {
     XI q -> XI (add1 p q);
     XO q -> XO (add1 p q);
     XH -> XI p};
   XH -> case y of {
          XI q -> XO (succ q);
          XO q -> XI q;
          XH -> XO XH}}

add_carry :: Positive -> Positive -> Positive
add_carry x y =
  case x of {
   XI p ->
    case y of {
     XI q -> XI (add_carry p q);
     XO q -> XO (add_carry p q);
     XH -> XI (succ p)};
   XO p ->
    case y of {
     XI q -> XO (add_carry p q);
     XO q -> XI (add1 p q);
     XH -> XO (succ p)};
   XH -> case y of {
          XI q -> XI (succ q);
          XO q -> XO (succ q);
          XH -> XI XH}}

pred_double :: Positive -> Positive
pred_double x =
  case x of {
   XI p -> XI (XO p);
   XO p -> XI (pred_double p);
   XH -> XH}

mul :: Positive -> Positive -> Positive
mul x y =
  case x of {
   XI p -> add1 y (XO (mul p y));
   XO p -> XO (mul p y);
   XH -> y}

iter_op :: (a1 -> a1 -> a1) -> Positive -> a1 -> a1
iter_op op p a =
  case p of {
   XI p0 -> op a (iter_op op p0 (op a a));
   XO p0 -> iter_op op p0 (op a a);
   XH -> a}

to_nat :: Positive -> Nat
to_nat x =
  iter_op add x (S O)

of_succ_nat :: Nat -> Positive
of_succ_nat n =
  case n of {
   O -> XH;
   S x -> succ (of_succ_nat x)}

double :: Z -> Z
double x =
  case x of {
   Z0 -> Z0;
   Zpos p -> Zpos (XO p);
   Zneg p -> Zneg (XO p)}

succ_double :: Z -> Z
succ_double x =
  case x of {
   Z0 -> Zpos XH;
   Zpos p -> Zpos (XI p);
   Zneg p -> Zneg (pred_double p)}

pred_double0 :: Z -> Z
pred_double0 x =
  case x of {
   Z0 -> Zneg XH;
   Zpos p -> Zpos (pred_double p);
   Zneg p -> Zneg (XI p)}

pos_sub :: Positive -> Positive -> Z
pos_sub x y =
  case x of {
   XI p ->
    case y of {
     XI q -> double (pos_sub p q);
     XO q -> succ_double (pos_sub p q);
     XH -> Zpos (XO p)};
   XO p ->
    case y of {
     XI q -> pred_double0 (pos_sub p q);
     XO q -> double (pos_sub p q);
     XH -> Zpos (pred_double p)};
   XH ->
    case y of {
     XI q -> Zneg (XO q);
     XO q -> Zneg (pred_double q);
     XH -> Z0}}

add2 :: Z -> Z -> Z
add2 x y =
  case x of {
   Z0 -> y;
   Zpos x' ->
    case y of {
     Z0 -> x;
     Zpos y' -> Zpos (add1 x' y');
     Zneg y' -> pos_sub x' y'};
   Zneg x' ->
    case y of {
     Z0 -> x;
     Zpos y' -> pos_sub y' x';
     Zneg y' -> Zneg (add1 x' y')}}

opp :: Z -> Z
opp x =
  case x of {
   Z0 -> Z0;
   Zpos x0 -> Zneg x0;
   Zneg x0 -> Zpos x0}

sub0 :: Z -> Z -> Z
sub0 m n =
  add2 m (opp n)

mul0 :: Z -> Z -> Z
mul0 x y =
  case x of {
   Z0 -> Z0;
   Zpos x' ->
    case y of {
     Z0 -> Z0;
     Zpos y' -> Zpos (mul x' y');
     Zneg y' -> Zneg (mul x' y')};
   Zneg x' ->
    case y of {
     Z0 -> Z0;
     Zpos y' -> Zneg (mul x' y');
     Zneg y' -> Zpos (mul x' y')}}

sgn :: Z -> Z
sgn z =
  case z of {
   Z0 -> Z0;
   Zpos _ -> Zpos XH;
   Zneg _ -> Zneg XH}

to_nat0 :: Z -> Nat
to_nat0 z =
  case z of {
   Zpos p -> to_nat p;
   _ -> O}

of_nat :: Nat -> Z
of_nat n =
  case n of {
   O -> Z0;
   S n0 -> Zpos (of_succ_nat n0)}

hd_error :: (([]) a1) -> Prelude.Maybe a1
hd_error l =
  case l of {
   ([]) -> Prelude.Nothing;
   (:) x _ -> Prelude.Just x}

tl :: (([]) a1) -> ([]) a1
tl l =
  case l of {
   ([]) -> ([]);
   (:) _ m -> m}

last :: (([]) a1) -> a1 -> a1
last l d =
  case l of {
   ([]) -> d;
   (:) a l0 -> case l0 of {
                ([]) -> a;
                (:) _ _ -> last l0 d}}

append :: Prelude.String -> Prelude.String -> Prelude.String
append s1 s2 =
  case s1 of {
   ([]) -> s2;
   (:) c s1' -> (:) c (append s1' s2)}

data Tree =
   TEmpty
 | T Prelude.String (([]) Tree)

data Willow =
   W Tree Nat Nat

tchildren :: Tree -> ([]) Tree
tchildren t =
  case t of {
   TEmpty -> ([]);
   T _ c -> c}

tlabel :: Tree -> Prelude.String
tlabel t =
  case t of {
   TEmpty -> "";
   T l _ -> l}

wtree :: Willow -> Tree
wtree w =
  case w of {
   W t _ _ -> t}

wminus :: Willow -> Nat
wminus w =
  case w of {
   W _ m _ -> m}

wplus :: Willow -> Nat
wplus w =
  case w of {
   W _ _ p -> p}

tlast :: (([]) Tree) -> Tree
tlast ts =
  last ts TEmpty

thead :: (([]) Tree) -> Tree
thead ts =
  case hd_error ts of {
   Prelude.Just t -> t;
   Prelude.Nothing -> TEmpty}

tinit :: (([]) Tree) -> ([]) Tree
tinit l =
  case l of {
   ([]) -> ([]);
   (:) a l0 -> case l0 of {
                ([]) -> ([]);
                (:) _ _ -> (:) a (tinit l0)}}

abs :: Z -> Nat
abs z =
  to_nat0 (mul0 z (sgn z))

unite :: Nat -> Tree -> Tree -> Tree
unite u t1 t2 =
  case u of {
   O -> T (append (tlabel t1) (tlabel t2))
    (app (tchildren t1) (tchildren t2));
   S u' -> T (append (tlabel t1) (tlabel t2))
    (app (tinit (tchildren t1))
      (app ((:) (unite u' (tlast (tchildren t1)) (thead (tchildren t2)))
        ([])) (tl (tchildren t2))))}

keepneg :: Nat -> Nat -> Tree -> Tree -> Tree
keepneg k u t1 t2 =
  case k of {
   O -> unite u t1 t2;
   S k' -> T (tlabel t2) ((:) (keepneg k' u t1 (thead (tchildren t2)))
    (tl (tchildren t2)))}

keeppos :: Nat -> Nat -> Tree -> Tree -> Tree
keeppos k u t1 t2 =
  case k of {
   O -> unite u t1 t2;
   S k' -> T (tlabel t1)
    (app (tinit (tchildren t1)) ((:) (keeppos k' u (tlast (tchildren t1)) t2)
      ([])))}

zip :: Z -> Nat -> Tree -> Tree -> Tree
zip k u t1 t2 =
  case k of {
   Z0 -> unite u t1 t2;
   Zpos _ -> keeppos (abs k) u t1 t2;
   Zneg _ -> keepneg (abs k) u t1 t2}

wcat :: Willow -> Willow -> Willow
wcat w1 w2 =
  let {t1 = wtree w1} in
  let {t2 = wtree w2} in
  let {p1 = wplus w1} in
  let {p2 = wplus w2} in
  let {m1 = wminus w1} in
  let {m2 = wminus w2} in
  let {k = sub0 (of_nat p1) (of_nat m2)} in
  let {u = min p1 m2} in
  let {m = add0 (sub m2 p1) m1} in
  let {p = add0 (sub p1 m2) p2} in W (zip k u t1 t2) m p

