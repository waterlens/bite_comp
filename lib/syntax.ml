open Types
open Printf

type prog = term

and value =
  [ `Unit
  | `Int of int
  | `Var of string
  | `Abs of [ `TVar of string ] list * [ `Var of string ] * term
  | `Cont of context
  | (* new in desugared syntax *)
    `DsAbs of
    [ `TVar of string ] list * [ `Var of string ] * [ `L of string ] list * term
  ]

and term =
  [ `Ann of term * ty
  | `Val of value
  | `If of term * term * term
  | `Tuple of term list
  | `Let of string * term * term
  | `Raise of [ `F of string ]
  | `App of term * eff list * term
  | `Proj of term * int
  | `TryWith of
    term
    * [ `F of string ]
    * [ `TVar of string ] list
    * [ `Var of string ]
    * [ `Var of string ]
    * term
  | `Resume of [ `Var of string ] * term
  | (* new in desugared syntax *)
    `DsRaise of [ `L of string ]
  | `DsApp of term * eff list * term * [ `L of string ] list
  | `DsTryWith of
    term
    * label
    * [ `TVar of string ] list
    * [ `Var of string ]
    * [ `Var of string ]
    * term ]

and context = unit

let aux_show_list f l = String.concat " " @@ List.map f l

let rec show_value : value -> string = function
  | `Unit -> "()"
  | `Int n -> string_of_int n
  | `Var s -> s
  | `Abs (x1, x2, x3) ->
      sprintf "Λ %s. λ %s. %s"
        (aux_show_list (fun (`TVar s) -> s) x1)
        (let (`Var s) = x2 in
         s)
        (show_term x3)
  | `Cont ctx -> show_context ctx
  (* new in desugared syntax *)
  | `DsAbs (x1, `Var s, l, x3) ->
      sprintf "Λ %s. λ %s. λL %s %s"
        (aux_show_list (fun (`TVar s) -> s) x1)
        s
        (aux_show_list (fun (`L l) -> l) l)
        (show_term x3)

and show_term = function
  | `Ann (tm, ty) -> sprintf "( %s : %s )" (show_term tm) (show_ty ty)
  | `Val v -> show_value v
  | `If (cond, t, f) ->
      sprintf "if %s then %s else %s" (show_term cond) (show_term t)
        (show_term f)
  | `Tuple tms ->
      if List.length tms = 1 then "(" ^ (show_term @@ List.hd tms) ^ ",)"
      else sprintf "(%s)" (String.concat ", " @@ List.map show_term tms)
  | `Let (x, t, s) -> sprintf "let %s = %s in %s" x (show_term t) (show_term s)
  | `Raise (`F f) -> "raise 𝔽 " ^ f
  | `App (tm1, eff, tm2) ->
      sprintf "%s [ %s ] %s" (show_term tm1)
        (aux_show_list show_eff eff)
        (show_term tm2)
  | `Proj (tm, n) -> sprintf "(%s.%d)" (show_term tm) n
  | `TryWith (tm1, `F f, tv, `Var x, `Var k, tm2) ->
      sprintf "try %s with 𝔽 %s Λ %s. λ %s. λ κ %s. %s" (show_term tm1) f
        (aux_show_list (fun (`TVar s) -> s) tv)
        x k (show_term tm2)
  | `Resume (`Var ctx, tm) -> sprintf "resume %s %s" ctx (show_term tm)
  (* new in desugared syntax *)
  | `DsRaise (`L l) -> "raise " ^ l
  | `DsApp (tm1, eff, tm2, labels) ->
      sprintf "(%s [ %s ] %s [ %s ])" (show_term tm1)
        (aux_show_list show_eff eff)
        (show_term tm2)
        (aux_show_list (fun (`L l) -> l) labels)
  | `DsTryWith (tm1, (`L l, `F f), tv, `Var x, `Var k, tm2) ->
      sprintf "try %s with %s = 𝔽 %s Λ %s. λ %s. λ κ %s. %s" (show_term tm1) l f
        (aux_show_list (fun (`TVar s) -> s) tv)
        x k (show_term tm2)

and show_context = function _ -> "⟦ ⟧"

let show_prog = show_term
