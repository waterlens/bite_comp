open Printf

type label = [ `L of string ] * [ `F of string ]

type ty =
  [ `TUnit
  | `TInt
  | `TForallArrow of [ `TVar of string ] list * ty * ty * eff list
  | `TCont of ty * eff list * ty * eff list
  | `TProd of ty list
  | `TRef of tvar ref
  | (* new in desugared syntax *)
    `TDsForallArrow of
    [ `TVar of string ] list * ty * ty * label list * eff list
  | `TDsCont of label list * ty * eff list * ty * eff list ]

and eff = [ `TVar of string | `F of string ]
and tvar = [ `TBound of int | `TLink of ty | `TVar of string ]

let aux_show_list f l = String.concat " " @@ List.map f l

let show_label : label -> string =
 fun (`L l, `F f) -> "( " ^ l ^ ": " ^ f ^ " )"

let rec show_ty : ty -> string = function
  | `TUnit -> "unit"
  | `TInt -> "int"
  | `TForallArrow (x1, x2, x3, x4) ->
      sprintf "∀ %s. %s -> %s [ %s ]"
        (aux_show_list (fun (`TVar s) -> s) x1)
        (show_ty x2) (show_ty x3)
        (aux_show_list show_eff x4)
  | `TCont (t1, e1, t2, e2) ->
      sprintf "cont %s [ %s ] ~> %s [ %s ]" (show_ty t1)
        (aux_show_list show_eff e1)
        (show_ty t2)
        (aux_show_list show_eff e2)
  | `TProd ts -> String.concat " * " @@ List.map show_ty ts
  | `TRef r -> show_tvar !r
  (* new in desugared syntax *)
  | `TDsCont (labels, t1, e1, t2, e2) ->
      sprintf "cont (∀ %s. %s [ %s ]) ~> %s [ %s ]" (show_ty t1)
        (aux_show_list show_label labels)
        (aux_show_list show_eff e1)
        (show_ty t2)
        (aux_show_list show_eff e2)
  | `TDsForallArrow (x1, x2, x3, labels, x4) ->
      sprintf "∀ %s. %s -> (∀ %s. %s [ %s ])"
        (aux_show_list (fun (`TVar s) -> s) x1)
        (show_ty x2)
        (aux_show_list show_label labels)
        (show_ty x3)
        (aux_show_list show_eff x4)

and show_eff = function `TVar s -> s | `F s -> "𝔽 " ^ s

and show_tvar = function
  | `TBound n -> "↑" ^ string_of_int n
  | `TLink _ -> "<ty>"
  | `TVar s -> s
