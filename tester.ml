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
let test_infer_match = infer(T.E.Let(E.Match((E.Pair((E.Int 1),(E.Var "a"))),"b","c"),(E.Var "d")))
let test_infer_match_error_1 = infer(T.E.Let(E.Match((E.Pair((E.Int 1),(E.Var "a"))),"x","y"),(E.Var "b")))
let test_infer_match_error_2 = infer(T.E.Let(E.Match((E.Var "a"),"b","c"),(E.Bool true)))

(*
      | E.Pair (e1, e2) -> Prod (infer g e1, infer g e2)
      | E.Let (E.Match (e1, x, y), e2) ->
         let t = infer g e1 in match t with
           |  Prod(t1, t2) -> infer ((x,t1)::(y, t2)::g) e2
           |  _ -> fail("e1 has to be a pair")
*)