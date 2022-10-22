%{
open Syntax
open Types
%}

%start prog
%type <term> prog

%%
prog:
| tm = term { tm }

tvar:
| e = "<id>" { let v : [ `TVar of string ] = `TVar e in v }

var:
| e = "<id>" { let v : [ `Var of string ] = `Var e in v }

eff:
| e = "<id>" { let v : eff = `TVar e in v }

type_with_eff:
| t = prod_ty
  "[" eff = list(eff) "]"
  { (t, eff) }

prod_ty:
| tys = separated_nonempty_list("*", ty)
  { if List.length tys = 1 then List.hd tys else `TProd tys }

ty:
| "unit" { let t: ty = `TUnit in t }
| "\\/" 
  tvars = nonempty_list(tvar)
  "."
  t1 = ty
  "->"
  te = type_with_eff
  { let t2, eff = te in
    let t: ty = `TForallArrow (tvars, t1, t2, eff) in
    t
  }
| "cont" t1 = type_with_eff "~>" t2 = type_with_eff
  { let t1, e1 = t1 in
    let t2, e2 = t2 in
    let t: ty = `TCont (t1, e1, t2, e2) in
    t
  }
| "(" t = ty ")"
  { t }

value:
| "(" ")" { `Unit }
| n = "<int>" { let v: value = `Int n in v }
| x = "<id>"  { let v: value = `Var x in v }
| "/\\"
  tvars = nonempty_list(tvar)
  "."
  "\\"
  v = var
  "."
  tm = term
  { 
    let v: value = `Abs (tvars, v, tm) in
    v
  }

term:
| "(" tm = term ":" ty = ty ")"
  { `Ann (tm, ty) }
| v = value
  { `Val v }
| "if" cond = term
  "then" br1 = term
  "else" br2 = term
  { `If (cond, br1, br2) }
| "(" tm = term "," tms = separated_list(",", term) ")"
  { `Tuple (tm :: tms)}
| "let" x = "<id>" "=" tm = term "in"
  s = term
  { `Let (x, tm, s) }
| "raise" f = "<id>"
  { `Raise (`F f) }
| "(" f = term "[" eff = list(eff) "]" args = term ")"
  { `App (f, eff, args) }
| "(" tm = term "." n = "<int>" ")"
  { `Proj (tm, n) }
| "try" tm1 = term
  "with" f = "<id>"
  "/\\"
  tvars = nonempty_list(tvar)
  "."
  "\\"
  v = var
  "."
  "\\"
  k = var
  "."
  tm2 = term
  { 
    `TryWith (tm1, `F f, tvars, v, k, tm2)
  }
| "resume" k = var tm = term
  {
    `Resume (k, tm)
  }