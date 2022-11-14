type var = Named of string | Annotated of var * ty

and ty =
  | TVar of var
  | TCtor of ctor
  | TApp of ty * ty
  | TForall of string * ty
  | TArrow of ty * ty

and core = binding list
and binding = Rec of (var * expr) list | NonRec of var * expr

and expr =
  | Var of var
  | Lam of var * expr
  | App of expr * expr list
  | Let of binding * expr
  | Case of expr * (pattern * expr) list
  | Mark of string * expr
  | Goto of string * expr
  | Ctor of ctor

and pattern = PVar of var | PCon of ctor * var list | PWild
and ctor = CtUnit | CtTrue | CtFalse | CtRawInt | CtHandle | CtReturn | CtTuple
[@@deriving show]

exception Exn of string
