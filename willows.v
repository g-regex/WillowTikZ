Require Import Coq.Arith.PeanoNat.
Require Import ZArith.
Require Import List.
Require Import Strings.String.

Open Scope Z_scope.

(* nat to be replaced with some monoid *)
Inductive tree : Type :=
    TEmpty : tree
  | T : string -> list tree -> tree.

Inductive willow : Type :=
W : tree -> nat -> nat -> willow.

(* Eliminators *)

Definition tchildren (t : tree) :=
 match t with
 | TEmpty => (nil : list tree)
 | T _ c => c
 end.

Definition tlabel (t : tree) :=
 match t with
 | TEmpty => EmptyString
 | T l _ => l
 end.

Definition wtree (w : willow) :=
  match w with
  | W t _ _ => t
  end.

Definition wminus (w : willow) :=
  match w with
  | W _ m _ => m
  end.

Definition wplus (w : willow) :=
  match w with
  | W _ _ p => p
  end.

Definition tlast (ts : list tree) :=
List.last ts TEmpty.

Definition thead (ts : list tree) :=
  match List.head ts with
  | Some t => t
  | None   => TEmpty
  end.

Fixpoint tinit (l:list tree) {struct l} : list tree :=
    match l with
      | nil => nil
      | a :: nil => nil
      | a :: l => a :: tinit l
    end.

Definition abs (z:Z) : nat := Z.to_nat (z * Z.sgn z).

Fixpoint unite (u : nat) (t1 : tree) (t2 : tree) : tree :=
  match u with
  | O    => T (append (tlabel t1) (tlabel t2)) ((tchildren t1) ++ (tchildren t2))
  | S u' => T (append (tlabel t1) (tlabel t2)) ((tinit (tchildren t1)) ++
              (unite u' (tlast (tchildren t1)) (thead (tchildren t2)) :: nil) ++
              (List.tail (tchildren t2)))
  end.

Fixpoint keepneg (k : nat) (u : nat) (t1 : tree) (t2 : tree) : tree :=
  match k with
  | S k' => T (tlabel t2) ((keepneg k' u t1 (thead (tchildren t2))) :: List.tail (tchildren t2))
  | O    => unite u t1 t2
  end.

Fixpoint keeppos (k : nat) (u : nat) (t1 : tree) (t2 : tree) : tree :=
  match k with
  | S k' => T (tlabel t1) ((tinit (tchildren t1)) ++ ((keeppos k' u (tlast (tchildren t1)) t2) :: nil))
  | O    => unite u t1 t2
  end.

Definition zip (k : Z) (u : nat) (t1 : tree) (t2 : tree) : tree :=
  match k with
  | Zneg _ => keepneg (abs k) u t1 t2
  | Z0     => unite u t1 t2
  | Zpos _ => keeppos (abs k) u t1 t2
  end.

Definition wcat (w1 : willow) (w2 : willow) : willow :=
  let t1 := wtree w1 in
  let t2 := wtree w2 in
  let p1 := wplus w1 in
  let p2 := wplus w2 in
  let m1 := wminus w1 in
  let m2 := wminus w2 in
  let k  := Z.sub (Z.of_nat p1) (Z.of_nat m2) in
  let u  := Nat.min p1 m2 in
  let m  := Nat.add (Nat.sub m2 p1) m1 in
  let p  := Nat.add (Nat.sub p1 m2) p2 in
  W (zip k u t1 t2) m p.

(* Require Coq.extraction.Extraction.
Require Import ExtrHaskellZNum.
Require Import ExtrHaskellNatNum. *)

Require Import ExtrHaskellString.

Require Import ExtrHaskellBasic.
Extraction Language Haskell.
Extraction "./Willows.hs" wcat.
