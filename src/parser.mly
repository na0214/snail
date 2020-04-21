%{
  open Typedef
  open Syntax
%}

%token <int*Syntax.pos_info> INT
%token <float*Syntax.pos_info> FLOAT
%token <string*Syntax.pos_info> STRING
%token <string*Syntax.pos_info> ID CONS VAR
%token <Syntax.pos_info> LPAREN RPAREN LBRAC RBRAC LCBRAC RCBRAC
%token <Syntax.pos_info> LET FUN IN REC TYPEDEF OF ASTE OR
%token <Syntax.pos_info> EQUAL LESS GREAT PERIOD COMMA COLON SEMICOLON ARROW
%token <Syntax.pos_info> EOF

%start snail_parse

%type <Syntax.snail_AST> snail_parse
%type <Syntax.toplevel> toplevel
%type <Syntax.term> term simple_term
%type <Syntax.argument> argument

%%

snail_parse:
  | list(toplevel) EOF
  {
    $1
  }

toplevel:
  | LET name = VAR arguments = list(argument) EQUAL t = term
  {
    LetDec(fst name,arguments,t,$1)
  }
  | TYPEDEF name = CONS typevars = list(argument) EQUAL typedec = separated_list(OR,type_declare)
  {
    TypeDef(fst name,typevars,typedec,$1)
  }

type_declare:
  | CONS cons_def = option(cons_declare)
  {
    (fst $1,cons_def)
  }

cons_declare:
  | OF type_expr
  {
    $2
  }

type_expr:
  | type_expr ASTE type_expr
  {
    $1 @*@ $3
  }
  | simple_type_expr
  {
    $1
  }
  | type_expr simple_type_expr
  {
    TyApp ($1,$2)
  }

simple_type_expr:
  | LPAREN type_expr RPAREN
  {
    $2
  }
  | VAR
  {
    TyVar (Tyvar (fst $1))
  } 
  | CONS
  {
    TyCons (Tycon (fst $1))
  }

argument:
  | VAR
  {
    fst $1
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
  | LET rec_flag = option(REC) name = VAR arguments = list(argument) EQUAL e1 = term IN e2 = term
  {
    Let((match rec_flag with Some _ -> true | _ -> false),fst name,"",arguments,e1,e2,$1)
  }
  | FUN arguments = list(argument) ARROW e = term
  {
    Fun(arguments,"",e,$1)
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
    Var(fst $1,"",snd $1)
  }
  | CONS
  {
    Cons(fst $1,snd $1)
  }