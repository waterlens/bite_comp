open Result
open Printf

type var = Named of string | Annotated of var * ty

and ty =
  | TVar of var
  | TCtor of ctor * ty list
  | TApp of ty * ty
  | TForall of string * ty
  | TArrow of ty * ty
  | TNever

and core = Core of binding
and binding = Rec of (var * expr) list | NonRec of (var * expr)

and expr =
  | Var of var
  | Lam of var * expr
  | App of expr * expr
  | Ctor of ctor * expr list
  | Let of binding * expr
  | Case of expr * (pattern * expr) list
  | Mark of var * expr
  | Goto of var * expr
  | Type of ty

and pattern = PVar of var | PCon of ctor * var list | PWild

and ctor =
  | CtUnit
  | CtTrue
  | CtFalse
  | CtInt of int
  | CtHandle
  | CtReturn
  | CtTuple
  | CtCont
  | CtUnion

type arity = Any | None | Number of int

exception Exn of string

let ctor_of_string = function
  | "unit" -> CtUnit
  | "return" -> CtReturn
  | "handle" -> CtHandle
  | "true" -> CtTrue
  | "false" -> CtFalse
  | "tuple" -> CtTuple
  | "cont" -> CtCont
  | "union" -> CtUnion
  | _ -> raise @@ Exn "not a ctor"

let string_of_ctor = function
  | CtUnit -> "#unit"
  | CtReturn -> "#return"
  | CtHandle -> "#handle"
  | CtTrue -> "#true"
  | CtFalse -> "#false"
  | CtTuple -> "#tuple"
  | CtCont -> "#cont"
  | CtUnion -> "#union"
  | CtInt n -> "#" ^ string_of_int n

let aux_show_list f l = String.concat " " @@ List.map f l

let rec show_ty = function
  | TVar var -> show_var var
  | TCtor (ctor, tys) ->
      sprintf "%s [%s]" (string_of_ctor ctor) (aux_show_list show_ty tys)
  | TApp (t1, t2) -> sprintf "(%s %s)" (show_ty t1) (show_ty t2)
  | TForall (name, ty) -> sprintf "\\%s. (%s)" name (show_ty ty)
  | TArrow (t1, t2) -> sprintf "%s -> %s" (show_ty t1) (show_ty t2)
  | TNever -> "!"

and show_var = function
  | Named s -> s
  | Annotated (s, ty) -> sprintf "(%s: %s)" (show_var s) (show_ty ty)

let arity_of_ctor = function
  | CtUnit -> None
  | CtTrue -> None
  | CtFalse -> None
  | CtInt _ -> None
  | CtHandle -> Any
  | CtReturn -> Number 1
  | CtTuple -> Any
  | CtCont -> Number 2
  | CtUnion -> Any

let arity_satisfy x = function
  | None when x = 0 -> true
  | Any -> true
  | Number y when x = y -> true
  | _ -> false

let ( let* ) = bind

let rec mapM f = function
  | [] -> ok []
  | a :: l ->
      let* r = f a in
      let* t = mapM f l in
      ok @@ (r :: t)

let rec map2M f l1 l2 =
  match (l1, l2) with
  | [], [] -> ok []
  | a1 :: l1, a2 :: l2 ->
      let* r = f a1 a2 in
      let* t = map2M f l1 l2 in
      ok @@ (r :: t)
  | _, _ -> invalid_arg "map2M"

type fail_reason = WrongInfo of string | LackInfo of string [@@deriving show]

let wrong_info x = WrongInfo x
let lack_info x = LackInfo x

let rec name_of_var = function
  | Named s -> s
  | Annotated (v, _) -> name_of_var v

let type_of_var ctx = function
  | Named s ->
      Option.to_result ~none:(lack_info @@ "can't determine the type of " ^ s)
      @@ List.assoc_opt s ctx
  | Annotated (_, ty) -> ok ty

let type_of ctx = function
  | TVar var as ty -> value ~default:ty @@ type_of_var ctx var
  | _ as ty -> ty

let rec equal_ty ctx t1 t2 =
  let ( =* ) = equal_ty ctx in
  match (t1, t2) with
  | TVar v1, TVar v2 when name_of_var v1 = name_of_var v2 -> true
  | _ -> (
      match (type_of ctx t1, type_of ctx t2) with
      | TCtor (c1, ts1), TCtor (c2, ts2) ->
          c1 = c2 && List.for_all2 ( =* ) ts1 ts2
      | TApp (t1, t2), TApp (t3, t4) -> t1 =* t3 && t2 =* t4
      | TForall (v1, t1), TForall (v2, t2) -> v1 = v2 && t1 =* t2
      | TArrow (t1, t2), TArrow (t3, t4) -> t1 =* t3 && t2 =* t4
      | TNever, TNever -> true
      | _ -> false)

let filter_never_type =
  List.filter (fun x -> match x with TNever -> false | _ -> true)

let rec exhaust ctx branches types exhausted =
  match branches with
  | [] ->
      if exhausted then (true, [])
      else if [] = filter_never_type types then (true, types)
      else (false, types)
  | (pat, _) :: xs -> (
      match pat with
      | PWild -> exhaust ctx xs types true
      | PVar _ -> exhaust ctx xs types true
      | PCon (ctor, _) ->
          exhaust ctx xs
            (List.filter
               (fun x ->
                 match x with
                 | TCtor (ctor', _) when ctor = ctor' -> false
                 | _ -> true)
               types)
            false)

let check_bind eq t1 t2 =
  match (t1, t2) with
  | (Ok t1 as t), Ok t2 when eq t1 t2 -> t
  | (Ok _ as t), Error (LackInfo _) -> t
  | Error (LackInfo _), (Ok _ as t) -> t
  | (Error _ as e), _ -> e
  | _, (Error _ as e) -> e
  | Ok _, Ok _ ->
      error
      @@ wrong_info
           "the annotated type is not equal with checked type of expression"

let rec type_of_expr ctx =
  let ( =* ) t1 t2 = equal_ty ctx t1 t2 in
  let check_bind = check_bind ( =* ) in
  function
  | Var v -> type_of_var ctx v
  | Lam (x, b) ->
      let* xt = type_of_var ctx x in
      let* bt = type_of_expr ((name_of_var x, xt) :: ctx) b in
      ok @@ TArrow (xt, bt)
  | App (x, y) -> (
      let* xt = type_of_expr ctx x in
      let xt = type_of ctx xt in
      match xt with
      | TArrow (a, b) ->
          let* ty = type_of_expr ctx y in
          if ty =* a then ok b else error @@ wrong_info "wrong type of argument"
      | _ -> error @@ wrong_info "not an applicable type")
  | Ctor (ctor, xs) ->
      if not @@ arity_satisfy (List.length xs) @@ arity_of_ctor ctor then
        error @@ wrong_info
        @@ sprintf "%s: wrong number of arguments" (string_of_ctor ctor)
      else
        let* xst = mapM (fun x -> type_of_expr ctx x) xs in
        ok @@ TCtor (ctor, xst)
  | Let (bind, e2) ->
      let* new_ctx = update_ctx_with_binding ctx bind in
      type_of_expr new_ctx e2
  | Case (expr, branches) -> (
      let* ty = type_of_expr ctx expr in
      let ty = type_of ctx ty in
      let check_pattern_bind ctx vars tys =
        let* tys =
          map2M check_bind
            (List.map (fun x -> type_of_var ctx x) vars)
            (List.map (fun x -> ok x) tys)
        in
        let bindings =
          List.map2 (fun var ty -> (name_of_var var, ty)) vars tys
        in
        ok @@ bindings @ ctx
      in
      let check_pattern_var_num vars tys =
        if List.length vars < List.length tys then
          error @@ wrong_info "pattern variables are not enough"
        else if List.length vars > List.length tys then
          error @@ wrong_info "pattern variables are more than needed"
        else ok ()
      in
      let bind_pattern ctx pat ty =
        match pat with
        | PWild -> ok ctx
        | PVar v -> ok @@ ((name_of_var v, ty) :: ctx)
        | PCon (ctor, vars) -> (
            match ty with
            | TCtor (ctor', tys) when ctor = ctor' ->
                let* _ = check_pattern_var_num vars tys in
                check_pattern_bind ctx vars tys
            | _ -> error @@ wrong_info "not a correct ctor type")
      in
      let bind_union_pattern ctx pat types =
        match pat with
        | PWild -> ok ctx
        | PVar v -> ok @@ ((name_of_var v, TCtor (CtUnion, types)) :: ctx)
        | PCon (ctor, vars) -> (
            let ty =
              List.find
                (fun ty ->
                  match ty with
                  | TCtor (ctor', _) when ctor = ctor' -> true
                  | _ -> false)
                types
            in
            match ty with
            | TCtor (ctor', tys) when ctor = ctor' ->
                let* _ = check_pattern_var_num vars tys in
                check_pattern_bind ctx vars tys
            | _ -> error @@ wrong_info "not a correct ctor type")
      in
      let check_branches_type_same f =
        let* brt =
          mapM
            (fun (pat, expr) ->
              let* new_ctx = f pat in
              type_of_expr new_ctx expr)
            branches
        in
        let brt' = filter_never_type brt in
        if brt' = [] then ok TNever
        else
          let t1 = List.hd brt' in
          if List.for_all (fun ty -> ty =* t1) brt' then ok t1
          else error @@ wrong_info "the types of branches are not all the same"
      in
      match ty with
      | TCtor (CtUnion, xs) ->
          let exhausted, remained = exhaust ctx branches xs false in
          if exhausted then
            if filter_never_type remained = [] then
              check_branches_type_same (fun pat ->
                  bind_union_pattern ctx pat xs)
            else error @@ wrong_info "unused cases"
          else
            error @@ wrong_info
            @@ sprintf "cases are not exhausted: %s\n expr type: %s"
                 (aux_show_list show_ty remained)
                 (aux_show_list show_ty xs)
      | TCtor _ as t ->
          let exhausted, remained = exhaust ctx branches [ t ] false in
          if exhausted then
            if filter_never_type remained = [] then
              check_branches_type_same (fun pat -> bind_pattern ctx pat t)
            else error @@ wrong_info "unused cases"
          else
            error @@ wrong_info
            @@ sprintf "cases are not exhausted: %s\n expr type: %s"
                 (aux_show_list show_ty remained)
                 (show_ty t)
      | TNever ->
          if branches = [] then ok TNever
          else error @@ wrong_info "can't match never type"
      | _ -> error @@ wrong_info "can't match these types")
  | Mark (var, exp) -> (
      let* label = type_of_var ctx var in
      let label = type_of ctx label in
      match label with
      | TCtor (CtCont, [ x; y ]) ->
          let new_ctx = (name_of_var var, label) :: ctx in
          let* ty = type_of_expr new_ctx exp in
          let* ty =
            if ty =* TNever then ok @@ TCtor (CtUnion, [ x ])
            else if TCtor (CtReturn, [ ty ]) =* y then
              ok @@ TCtor (CtUnion, [ x; TCtor (CtReturn, [ ty ]) ])
            else
              error @@ wrong_info
              @@ sprintf
                   "%s: label has wrong return type: expr type is %s but label \
                    is %s"
                   (name_of_var var) (show_ty ty) (show_ty label)
          in
          ok ty
      | _ ->
          error @@ wrong_info
          @@ sprintf "%s: label should be annotated with cont type"
               (name_of_var var))
  | Goto (var, exp) ->
      let* label = type_of_var ctx var in
      let label = type_of ctx label in
      let* _ =
        match label with
        | TCtor (CtCont, x :: _) ->
            let* ty = type_of_expr ctx exp in
            if ty =* x then ok ty
            else
              error @@ wrong_info
              @@ sprintf
                   "go to wrong label: checked expr type: %s\nannotated: %s\n"
                   (show_ty ty) (show_ty x)
        | _ ->
            error @@ wrong_info
            @@ sprintf "%s: label should be annotated with cont type"
                 (name_of_var var)
      in
      ok TNever
  | Type ty -> ok ty

and update_ctx_with_binding ctx bind =
  let ( =* ) t1 t2 = equal_ty ctx t1 t2 in
  let check_bind = check_bind ( =* ) in
  match bind with
  | NonRec (x, e1) ->
      let* t = check_bind (type_of_var ctx x) (type_of_expr ctx e1) in
      ok ((name_of_var x, t) :: ctx)
  | Rec bindings ->
      let rec type_bindings acc = function
        | [] -> ok @@ List.rev acc
        | (var, _) :: xs ->
            let* ty = type_of_var ctx var in
            let name = name_of_var var in
            let* acc = type_bindings ((name, ty) :: acc) xs in
            ok acc
      in
      let rec expr_types acc ctx = function
        | [] -> List.rev acc
        | (_, exp) :: xs ->
            let ty = type_of_expr ctx exp in
            let acc = expr_types (ty :: acc) ctx xs in
            acc
      in
      let* type_bindings = type_bindings [] bindings in
      let new_ctx = type_bindings @ ctx in
      let expr_types = expr_types [] new_ctx bindings in
      let* _ =
        map2M (fun (_, t1) t2 -> check_bind (Ok t1) t2) type_bindings expr_types
      in
      ok new_ctx

let emit_binding =
  let emit_binding_declaration (_, exp) =
    match exp with
    | Lam _ -> ()
    | _ -> raise @@ Exn "a global binding must be a lambda abstraction"
  in
  let emit_binding_impl _ = () in
  function
  | Rec bindings ->
      List.iter emit_binding_declaration bindings;
      List.iter emit_binding_impl bindings
  | NonRec binding -> emit_binding_impl binding

let check_core core =
  let ctx = [] in
  let (Core binding) = core in
  match update_ctx_with_binding ctx binding with
  | Ok _ -> ()
  | Error reason -> printf "%s" @@ show_fail_reason reason