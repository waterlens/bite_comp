open Printf
type ctype = CInt | CVoid | CFunc of ctype * ctype list
let rec show_ctype = function
| CInt -> "int"
| CVoid -> "void"
| CFunc (r, a) ->
    sprintf "%s(*)(%s)" (show_ctype r)
      (String.concat "," @@ List.map show_ctype a)

type cstatement =
| If of cexpr * cstatement * cstatement option
| Return of cexpr
| Block of cstatement list
| Expr of cexpr

and cexpr = Call of cexpr * cexpr list | Var of string

let rec show_cstatement = function
| If (cond, t, Some f) ->
    sprintf "if (%s) %s else %s" (show_cexpr cond)
      (show_cstatement @@ Block [ t ])
      (show_cstatement @@ Block [ f ])
| If (cond, t, None) ->
    sprintf "if (%s) %s" (show_cexpr cond) (show_cstatement @@ Block [ t ])
| Return e -> sprintf "return %s;" (show_cexpr e)
| Expr e -> sprintf "%s;" @@ show_cexpr e
| Block blk -> String.concat " " @@ List.map show_cstatement blk

and show_cexpr = function
| Var s -> s
| Call (e, args) ->
    sprintf "%s(%s)" (show_cexpr e)
      (String.concat "," @@ List.map show_cexpr args)

type cfunction = {
name : string;
return : ctype;
parameters : (string * ctype) list;
statements : cstatement list;
}

let show_cfunction { name; return; parameters; statements } =
sprintf "%s %s(%s){%s}" name (show_ctype return)
  (String.concat ","
  @@ List.map (fun (name, ty) -> show_ctype ty ^ " " ^ name) parameters)
  (String.concat " " @@ List.map show_cstatement statements)

let make_cfunction name return parameters statements =
{ name; return; parameters; statements }

type csource = cfunction list