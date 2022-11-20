open Printf

type ctype =
  | Int
  | Void
  | Func of ctype * ctype list
  | Ptr of ctype
  | RawType of string

let rec show_ctype = function
  | Int -> "int"
  | Void -> "void"
  | Func (r, a) ->
      sprintf "%s(*)(%s)" (show_ctype r)
        (String.concat "," @@ List.map show_ctype a)
  | Ptr ct -> show_ctype ct ^ "*"
  | RawType s -> s

type cstatement =
  | If of cexpr * cstatement * cstatement option
  | Return of cexpr
  | Block of cstatement list
  | Expr of cexpr
  | Decl of string * ctype * cexpr option
  | Switch of cexpr * (cexpr option * cstatement) list
  | Break

and cliteral = LInt of int | LBool of bool

and cexpr =
  | Call of cexpr * cexpr list
  | Var of string
  | Lit of cliteral
  | RawExpr of string
  | Binop of string * cexpr * cexpr

let int = Int
let void = Void
let ptr t = Ptr t
let func r a = Func (r, a)
let if' cond tru els = If (cond, tru, els)
let return e = Return e
let block xs = Block xs
let expr e = Expr e
let decl n t i = Decl (n, t, i)
let switch e brs = Switch (e, brs)
let break = Break
let lit x = Lit x
let lint n = LInt n
let lbool b = LBool b
let call x xs = Call (x, xs)
let var s = Var s
let binop s a b = Binop (s, a, b)
let asgn a b = Binop ("=", a, b)

type cfunction = {
  name : string;
  return : ctype;
  parameters : (string option * ctype) list;
  statements : cstatement list option;
}

let make_cfunction name return parameters statements =
  { name; return; parameters; statements = Some statements }

let make_cfunction_declaration name return parameters =
  { name; return; parameters; statements = None }

type csource = cfunction list

open Format

let rec pp_cstatement = function
  | If (cond, t, Some f) ->
      sprintf "if (%s)%selse%s" (pp_cexpr cond)
        (pp_cstatement @@ Block [ t ])
        (pp_cstatement @@ Block [ f ])
  | If (cond, t, None) ->
      sprintf "if (%s)%s" (pp_cexpr cond) (pp_cstatement @@ Block [ t ])
  | Return e -> sprintf "return %s;" (pp_cexpr e)
  | Expr e -> sprintf "%s;" @@ pp_cexpr e
  | Block blk ->
      sprintf "{%s}" @@ String.concat " " @@ List.map pp_cstatement blk
  | Decl (n, t, i) ->
      sprintf "%s %s%s;" (show_ctype t) n
        (match i with Some expr -> " = " ^ pp_cexpr expr | _ -> "")
  | Switch (e, brs) ->
      String.concat ""
        [
          sprintf "switch (%s){" (pp_cexpr e);
          String.concat ""
          @@ List.map
               (fun (l, st) ->
                 match l with
                 | Some l ->
                     sprintf "case %s: %s" (pp_cexpr l) (pp_cstatement st)
                 | _ -> sprintf "default: %s" (pp_cstatement st))
               brs;
          sprintf "}";
        ]
  | Break -> "break;"

and pp_cexpr = function
  | Var s -> s
  | RawExpr s -> s
  | Call (e, args) ->
      sprintf "%s(%s)" (pp_cexpr e)
        (String.concat (sprintf ", ") @@ List.map pp_cexpr args)
  | Lit (LBool true) -> "1"
  | Lit (LBool false) -> "0"
  | Lit (LInt n) -> sprintf "%dll" n
  | Binop (s, a, b) -> sprintf "%s %s %s" (pp_cexpr a) s (pp_cexpr b)

let pp_cfunction { name; return; parameters; statements } =
  String.concat ""
    [
      sprintf "%s %s(" (show_ctype return) name;
      sprintf "%s"
        (String.concat (sprintf ", ")
        @@ List.map
             (fun (name, ty) ->
               sprintf "%s%s" (show_ctype ty)
                 (match name with
                 | Some name -> sprintf " %s" name
                 | None -> ""))
             parameters);
      (match statements with
      | Some stmts -> sprintf ")%s" @@ pp_cstatement @@ block stmts
      | _ -> sprintf ");");
    ]

let pp_csource src = String.concat "" @@ List.map pp_cfunction src

let substitute_placeholders_with s xs =
  let ph_pat = Str.regexp {|#\([0-9]+\)|} in
  let ph_pat' = Str.regexp {|#@|} in
  let buf = Buffer.create 32 in
  Array.iteri
    (fun n x ->
      if n <> 0 then Buffer.add_string buf ", ";
      Buffer.add_string buf @@ pp_cexpr x)
    xs;
  let array =
    sprintf "(void *[%d]){%s}" (Array.length xs)
      (String.of_bytes @@ Buffer.to_bytes buf)
  in
  let s =
    Str.global_substitute ph_pat
      (fun s ->
        let n = int_of_string @@ Str.matched_group 1 s in
        pp_cexpr xs.(n))
      s
  in
  Str.global_replace ph_pat' array s

let raw_type t xs = RawType (substitute_placeholders_with t xs)
let raw_expr s xs = RawExpr (substitute_placeholders_with s xs)