{
open Core_tokens
open Lexing

type error =
  | UnexpectedToken of string
  | UnexpectedEof of string 
  | InvalidStringLiteral

exception Error of error * position

let convert_escaped c =
  match c with
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | _   -> c

let keywords_table = Hashtbl.of_seq @@ List.to_seq
    [ ("let"        , TK_LET)
    ; ("in"         , TK_IN)
    ; ("type"       , TK_TYPE)
    ; ("case"       , TK_CASE)
    ; ("of"         , TK_OF)
    ; ("mark"       , TK_MARK)
    ; ("goto"       , TK_GOTO)
    ; ("end"        , TK_END)
    ]

let sops_table = Hashtbl.of_seq @@ List.to_seq
    [ ('.'  , TK_DOT)
    ; (','  , TK_COMMA)
    ; (':'  , TK_COLON)
    ; (';'  , TK_SEMICOLON)
    ; ('^'  , TK_CARET)
    ; ('_'  , TK_UNDERSCORE)
    ; ('#'  , TK_HASH)
    ; ('('  , TK_L_PAREN)
    ; (')'  , TK_R_PAREN)
    ; ('{'  , TK_L_BRACE)
    ; ('}'  , TK_R_BRACE)
    ; ('['  , TK_L_BRACKET)
    ; (']'  , TK_R_BRACKET)
    ; ('+'  , TK_ADD)
    ; ('-'  , TK_SUB)
    ; ('*'  , TK_MUL)
    ; ('/'  , TK_DIV)
    ; ('%'  , TK_MOD)
    ; ('='  , TK_ASGN)
    ; ('<'  , TK_LT)
    ; ('>'  , TK_GT)
    ; ('!'  , TK_LNOT)
    ; ('|'  , TK_OR)
    ; ('\'' , TK_SQUOTE)
    ; ('~'  , TK_TILDE)
    ; ('\\' , TK_BACKSLASH)
    ]

let mops_table = Hashtbl.of_seq @@ List.to_seq
    [ ("<-"   , TK_L_ARROW)
    ; ("->"   , TK_R_ARROW)
    ; ("=>"   , TK_D_ARROW)
    ; ("~>"   , TK_C_ARROW)
    ; ("=="   , TK_EQ)
    ; ("!="   , TK_NEQ)
    ; (">="   , TK_GE)
    ; ("<="   , TK_LE)
    ; ("&&"   , TK_LAND)
    ; ("||"   , TK_LOR)
    ; ("/\\"  , TK_BLAMBDA)
    ; ("\\/"  , TK_FORALL)
    ]
}

let newline       = "\n"
let whitespace    = ['\t' ' ']+
let ddigit        = ['0'-'9']
let hdigit        = ['0'-'9' 'a'-'f' 'A'-'F']
let alpha         = ['a'-'z' 'A'-'Z']
let alphanum      = alpha | ddigit
let identfier     = (alpha | '_') (alphanum | '_') *  
let int_lit       = ddigit+
let escaped       = ['\\' '\'' '\"' 'n' 't' 'r' ' ']
let bool_lit      = "true" | "false"
let sops          = ['.' ',' ':' ';' '^' '_' '#' '(' ')' '{' '}' '[' ']' '+' '-' '*' '/' '%' '=' '>' '<' '!' '|' '\'' '~' '\\']
let mops          = "<-" | "->" | "=>" | "==" | "!=" | ">=" | "<=" | "&&" | "||" | "/\\" | "\\/"

rule tokenize = parse
  | eof             { TK_EOF }
  | whitespace      { tokenize lexbuf }
  | newline         { new_line lexbuf; tokenize lexbuf }
  | "//"            { comment lexbuf }
  | "/*"            { long_comment lexbuf }
  | int_lit   as x  { TK_INT_LITERAL (int_of_string x) }
  | bool_lit  as x  { TK_BOOL_LITERAL (bool_of_string x) }
  | identfier as x  { 
                      match Hashtbl.find_opt keywords_table x with
                      | Some token -> token
                      | None       -> TK_ID x
                    }
  | mops as op      { Option.get @@ Hashtbl.find_opt mops_table op}
  | sops as op      { Option.get @@ Hashtbl.find_opt sops_table op }
  | _               { raise @@ Error (UnexpectedToken "unexpected token", lexeme_start_p lexbuf) }

and comment = parse
  | newline     { new_line lexbuf; tokenize lexbuf }
  | _           { comment lexbuf }

and long_comment = parse
  | newline     { new_line lexbuf; long_comment lexbuf }
  | "*/"        { tokenize lexbuf }
  | _           { long_comment lexbuf }

and string buffer = parse
  | '\"'                { () }
  | '\\' (escaped as c) { Buffer.add_char buffer (convert_escaped c) ; string buffer lexbuf }
  | '\\' _ | newline    { raise @@ Error (InvalidStringLiteral, lexeme_start_p lexbuf) }
  | _ as c              { Buffer.add_char buffer c; string buffer lexbuf }
  | eof                 { raise @@ Error (UnexpectedEof "string literal doesn't terminate", lexeme_start_p lexbuf)}