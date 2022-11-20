%{
open Core
%}

%start start
%type <core> start

%%

global_binding:
| "let" bd = binding "end" { bd }

start:
| bds = nonempty_list(global_binding) "<eof>" { Core bds }

var: 
| s = "<id>" { Named s }
| "(" s = "<id>" ":" ty = ty ")" { Annotated (Named s, ty) }

ctor:
| "#" s = "<id>"
  { 
    ctor_of_string s
  }

tlit:
| "%" s = "<id>"
  { 
    tlit_of_string s
  }

lit:
| n = "<int>"
  {
    Lit (Int n)
  }
| n = "<bool>"
  {
    Lit (Bool n)
  }

ty:
| ty = ty_arrow { ty }

ty_arrow:
| ty1 = ty_forall
  {
    ty1
  }
| ty1 = ty_forall "->" tys = separated_nonempty_list("->", ty_forall)
  {
    List.fold_left (fun x y -> TArrow (x, y)) ty1 tys
  }

ty_forall:
| "\\" s = "<id>" "." ty = ty_app { TForall (s, ty) }
| ty = ty_app { ty }

ty_app:
| ty = ty_atom { ty } 
| "(" t1 = ty_atom tys = nonempty_list(ty_atom) ")" 
  {
    List.fold_right (fun x y -> TApp (y, x)) tys t1 
  }

ty_atom:
| s = "<id>" { TVar (Named s) }
| ct = ctor { TCtor (ct, []) }
| ct = ctor "[" tys = list(ty) "]" { TCtor (ct, tys) }
| "(" ty = ty_arrow ")" { ty }
| "!" { TNever }
| tl = tlit { TLit tl }

simple_binding:
| v = var "=" e = expr { (v, e) }

type_binding:
| v = var "=" "type" t = ty { (v, t) }

binding:
| bds = separated_nonempty_list(";", simple_binding)
  {
    match bds with
    | [x] -> let (a, b) = x in NonRec (a, b)
    | _ -> Rec bds
  }
| bds = separated_nonempty_list(";", type_binding)
  {
    RecType bds
  }

expr:
| e = expr_atom { e }
| "(" f = expr_atom a = expr_atom ")" { App (f, a) }
| "(" ct = ctor xs = nonempty_list(expr_atom) ")" { Ctor (ct, xs) }
| ct = ctor { Ctor (ct, []) }

expr_atom:
| li = lit { li }
| "(" e = expr ")" { e }
| v = var { Var v }
| "\\" v = var "." x = expr { Lam (v, x) }
| "let" bd = binding "in" e = expr { Let(bd, e) }
| "mark" v = var "in" e = expr { Mark(v, e) }
| "goto" v = var "(" e = expr ")" { Goto(v, e) }
| "case" e = expr "of" brs = list(branch) "end" { Case (e, brs) } 

branch:
| "|" p = pattern "->" e = expr { (p, e) }

pattern:
| ct = ctor "(" vs = nonempty_list(var) ")" { PCon (ct, vs) }
| ct = ctor { PCon (ct, []) }
| "_" { PWild }