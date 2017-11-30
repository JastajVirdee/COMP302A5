#use "hw5.ml"

module E = Exp 
module T = Types

let exp = E.Let (E.Val(E.Var("c"), "a"), E.Var("b"))

let sub = E.subst

let test1 = sub (E.Var("sad"), "a") exp
let test2 = sub (E.Var("sad"), "c") exp

let match_exp = E.Let (E.Match(E.Var "c", "a", "b"), E.Var "abc")

(* Test to see if expressions are replaced in regular Let *)
let test3 = sub (E.Var("notsad"), "a") exp
let test4 = sub (E.Var("notsad"), "b") exp
let test5 = sub (E.Var("notsad"), "c") exp

(* Test to see if expressions are replaced in Match Let *)
let test6 = sub (E.Var("notsad"), "a") match_exp
let test7 = sub (E.Var("notsad"), "b") match_exp
let test8 = sub (E.Var("notsad"), "c") match_exp
let test9 = sub (E.Var("notsad"), "abc") match_exp

(* Test for Infer *)
let infer = T.infer [("b", T.Bool);("a", T.Int)]
let test_infer = infer (T.E.Var("a"))
let test_infer_if = infer(T.E.If((E.Var "b"),(E.Bool true),(E.Var "b")))
let test_infer_pair_1 = infer(T.E.Pair((E.Var "b"),(E.Bool true)))
let test_infer_pair_2 = infer(T.E.Pair((E.Var "b"),(E.Var "a")))
let test_infer_match_int = infer(T.E.Let(E.Match((E.Pair((E.Int 1),(E.Var "b"))),"x","y"),(E.Var "a")))
let test_infer_match_bool = infer(T.E.Let(E.Match((E.Pair((E.Int 1),(E.Var "a"))),"x","y"),(E.Var "b")))
(*let test_infer_match_error_2 = infer(T.E.Let(E.Match((E.Var "a"),"b","c"),(E.Bool true))) *)

(*
      | E.Pair (e1, e2) -> Prod (infer g e1, infer g e2)
      | E.Let (E.Match (e1, x, y), e2) ->
         let t = infer g e1 in match t with
           |  Prod(t1, t2) -> infer ((x,t1)::(y, t2)::g) e2
           |  _ -> fail("e1 has to be a pair")
*)

module E = Exp
module D = DeadCode
module R = RemoveLetMatch
module Ev = Eval

let case1 = E.Let (E.Val (E.Int 7, "x"), E.Int 9)
let testCase1 = D.optimize case1


let case2 = E.Let (E.Val (E.Int 7, "x"), E.Var "x")
let testCase2 = D.optimize case2


let case3 = E.Let (E.Match (E.Pair(E.Int 3, E.Bool false), "x", "y"), E.If (E.Var "y", E.Var "x", E.Int 7))
let testCase3 = D.optimize case3


let case4 = E.Let (E.Match (E.Pair(E.Int 3, E.Bool false), "x", "y"), E.Var "x")
let testCase4 = D.optimize case4


let case5 = E.Let (E.Match (E.Pair(E.Int 3, E.Bool false), "x", "y"), E.If (E.Bool false, E.Int 1, E.Int 7))
let testCase5 = D.optimize case5


let case6 = E.Let (E.Val (E.Int 3, "z"), E.Let (E.Val (E.Int 7, "x"), E.Let (E.Val (E.Var "x", "y"), E.Var "z")))
let testCase6 = D.optimize case6


let case7 = E.Let (E.Match (E.Pair(E.Int 5, E.Int 7), "x", "y"), E.Let (E.Val (E.Int 2, "a"), E.Primop(E.Plus, [E.Var "a"; E.Var "y"])))
let testCase7 = R.optimize case7
let testCase7_1 = Ev.eval case7

