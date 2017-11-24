let exp = E.Let (E.Val(E.Var("c"), "a"), E.Var("b"))

let sub = E.subst

let test1 = sub (E.Var("sad"), "a") exp
let test2 = sub (E.Var("sad"), "c") exp

let match_exp = E.Let (E.Match(E.Var "c", "a", "b"), E.Var "abc")

let test3 = sub (E.Var("notsad"), "a") exp
let test4 = sub (E.Var("notsad"), "c") exp
