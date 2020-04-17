%{
  open Syntax
%}

%token <int*Syntax.pos_info> INT
%token <float*Syntax.pos_info> FLOAT
%token <string*Syntax.pos_info> STRING
%token <string*Syntax.pos_info> ID CONS VAR
%token <Syntax.pos_info> LPAREN RPAREN LBRAC RBRAC LCBRAC RCBRAC
%token <Syntax.pos_info> LET FUN IN
%token <Syntax.pos_info> EQUAL LESS GREAT PERIOD COMMA COLON SEMICOLON ARROW
%token <Syntax.pos_info> EOF

%start snail_parse

%type <Syntax.term> term simple_term snail_parse

%%

snail_parse:
  | term EOF
  {
    $1
  }

term:
  simple_term
  {
    $1
  }
  | term simple_term
  {
    App($1,$2)
  }
  | LET name = VAR EQUAL e1 = term IN e2 = term
  {
    Let(fst name,e1,e2,$1)
  }
  | FUN name = VAR ARROW e = term
  {
    Fun(fst name,e,$1)
  }

simple_term:
  | LPAREN e = term RPAREN
  {
    e
  }
  | INT
  {
    IntLit(fst $1,snd $1)
  }
  | FLOAT
  {
    FloatLit(fst $1,snd $1)
  }
  | STRING
  {
    StringLit(fst $1,snd $1)
  }
  | VAR
  {
    Var(fst $1,snd $1)
  }
  | CONS
  {
    Cons(fst $1,snd $1)
  }