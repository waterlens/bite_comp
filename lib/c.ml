open Printf

type ctype =
  | Int
  | Void
  | Func of ctype * ctype list
  | Ptr of ctype
  | InlineType of string

let rec show_ctype = function
  | Int -> "int"
  | Void -> "void"
  | Func (r, a) ->
      sprintf "%s(*)(%s)" (show_ctype r)
        (String.concat "," @@ List.map show_ctype a)
  | Ptr ct -> show_ctype ct ^ "*"
  | InlineType s -> s

type cstatement =
  | If of cexpr * cstatement * cstatement option
  | Return of cexpr
  | Block of cstatement list
  | Expr of cexpr
  | Decl of string * ctype * cexpr option

and cliteral = LInt of int | LBool of bool

and cexpr =
  | Call of cexpr * cexpr list
  | Var of string
  | Lit of cliteral
  | InlineExpr of string
  | Binop of string * cexpr * cexpr

let int = Int
let void = Void
let ptr t = Ptr t
let inline_type t = InlineType t
let func r a = Func (r, a)
let if' cond tru els = If (cond, tru, els)
let return e = Return e
let block xs = Block xs
let expr e = Expr e
let decl n t i = Decl (n, t, i)
let lit x = Lit x
let lint n = LInt n
let lbool b = LBool b
let call x xs = Call (x, xs)
let var s = Var s
let inline_expr s = InlineExpr s
let binop s a b = Binop (s, a, b)
let asgn a b = Binop ("=", a, b)

let rec show_cstatement = function
  | If (cond, t, Some f) ->
      sprintf "if (%s)\n%s\nelse\n%s\n" (show_cexpr cond)
        (show_cstatement @@ Block [ t ])
        (show_cstatement @@ Block [ f ])
  | If (cond, t, None) ->
      sprintf "if (%s)\n%s" (show_cexpr cond) (show_cstatement @@ Block [ t ])
  | Return e -> sprintf "return %s;" (show_cexpr e)
  | Expr e -> sprintf "%s;" @@ show_cexpr e
  | Block blk -> String.concat " " @@ List.map show_cstatement blk
  | Decl (n, t, i) ->
      sprintf "%s %s%s;" (show_ctype t) n
        (match i with Some expr -> " = " ^ show_cexpr expr | _ -> "")

and show_cexpr = function
  | Var s -> s
  | InlineExpr s -> s
  | Call (e, args) ->
      sprintf "%s(%s)" (show_cexpr e)
        (String.concat "," @@ List.map show_cexpr args)
  | Lit (LBool true) -> "1"
  | Lit (LBool false) -> "0"
  | Lit (LInt n) -> string_of_int n ^ "ll"
  | Binop (s, a, b) -> sprintf "%s %s %s" (show_cexpr a) s (show_cexpr b)

type cfunction = {
  name : string;
  return : ctype;
  parameters : (string option * ctype) list;
  statements : cstatement list option;
}

let show_cfunction { name; return; parameters; statements } =
  sprintf "%s %s(%s)%s\n" (show_ctype return) name
    (String.concat ","
    @@ List.map
         (fun (name, ty) ->
           show_ctype ty
           ^ match name with Some name -> " " ^ name | None -> "")
         parameters)
    (match statements with
    | Some statements ->
        "{" ^ (String.concat " " @@ List.map show_cstatement statements) ^ "}"
    | None -> ";")

let make_cfunction name return parameters statements =
  { name; return; parameters; statements = Some statements }

let make_cfunction_declaration name return parameters =
  { name; return; parameters; statements = None }

type csource = cfunction list

let show_csource src = String.concat "" @@ List.map show_cfunction src
