{
  open Parser
  open Lexing
  open Syntax

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                   pos_lnum = pos.pos_lnum + 1
        }

  let print_position outx lexbuf =
    let pos = lexbuf.lex_curr_p in
      Core.fprintf outx "File \"%s\", line %d, position %d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

  let position_to_string pos =
    Core.sprintf "File \"%s\", line %d, position %d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
}

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E']['-' '+']? digit+
let int = digit*
let float = digit* frac? exp?
let white = [' ' '\t']+
let newline = '\r'|'\n'|"\r\n"
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let cons = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let var = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let op = [':' '@' '#' '&' '%' '*' '-' '<' '>' '$' '+' '=' '^' '~' '|' '/']*
let binop1_l = ['*' '/' '%'] op+
let binop2_l = ['+' '-'] op+
let binop3_r = [':'] op+
let binop4_r = ['@' '^'] op+
let binop5_l = ['=' '<' '>' '|' '&'] op+
let binop6_r = ['$'] op+
let prefixop = ['!' '~' '?'] op+

rule token = parse
  | white {token lexbuf}
  | newline {next_line lexbuf; token lexbuf}
  | int {INT (int_of_string (Lexing.lexeme lexbuf),translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | float {FLOAT (float_of_string(Lexing.lexeme lexbuf),translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | '"' {read_string (Buffer.create 17) lexbuf}
  | "[" {LBRAC(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "]" {RBRAC(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "(" {LPAREN(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | ")" {RPAREN(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "{" {LCBRAC(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "}" {RCBRAC(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "<" {LESS(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | ">" {GREAT(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "=" {EQUAL(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "." {PERIOD(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "," {COMMA(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | ":" {COLON(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | ";" {SEMICOLON(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "*" {ASTE(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "|" {OR(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "->" {ARROW(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "let" {LET(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "fun" {FUN(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "in" {IN(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "rec" {REC(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "of" {OF(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "typedef" {TYPEDEF(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "match" {MATCH(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "with" {WITH(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "end" {END(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | "()" {UNIT(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | binop1_l {BINOP1L(Lexing.lexeme lexbuf,translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | binop2_l {BINOP2L(Lexing.lexeme lexbuf,translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | binop3_r {BINOP3R (Lexing.lexeme lexbuf,translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | binop4_r {BINOP4R (Lexing.lexeme lexbuf,translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | binop5_l {BINOP5L (Lexing.lexeme lexbuf,translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | binop6_r {BINOP6R (Lexing.lexeme lexbuf,translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | cons {CONS(Lexing.lexeme lexbuf,translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | var {VAR(Lexing.lexeme lexbuf,translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | id {ID (Lexing.lexeme lexbuf,translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | eof {EOF(translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
and read_string buf = parse
  | '"' {STRING (Buffer.contents buf,translate_lexbuf_to_pos_info lexbuf.lex_curr_p)}
  | '\\' 't' {Buffer.add_char buf '\t'; read_string buf lexbuf}
  | '\\' 'b' {Buffer.add_char buf '\b'; read_string buf lexbuf}
  | '\\' 'r' {Buffer.add_char buf '\r'; read_string buf lexbuf}
  | '\\' 'n' {Buffer.add_char buf '\n'; read_string buf lexbuf}
  | '\\' '\\' {Buffer.add_char buf '\\'; read_string buf lexbuf}
  | [^ '"' '\\']+ {Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}
  | _ {raise (SyntaxError ("illegal string character: " ^ Lexing.lexeme lexbuf))}
  | eof {raise (SyntaxError ("String is not terminated"))}
