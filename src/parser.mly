%{
  open Typedef
  open Syntax

  let translate_multi_tuple_to_pair tup_list pos =
  match tup_list with
   [] -> Cons (",",None,pos)
  | x :: xs -> List.fold_left (fun acc -> fun a -> Prod(acc,a,pos)) x xs
%}

%token <int*Syntax.pos_info> INT
%token <float*Syntax.pos_info> FLOAT
%token <string*Syntax.pos_info> STRING
%token <string*Syntax.pos_info> ID CONS VAR BINOP1L BINOP2L BINOP3R BINOP4R BINOP5L BINOP6R
%token <Syntax.pos_info> LPAREN RPAREN LBRAC RBRAC LCBRAC RCBRAC
%token <Syntax.pos_info> LET FUN IN REC TYPEDEF OF ASTE OR
%token <Syntax.pos_info> MATCH WITH END
%token <Syntax.pos_info> EQUAL LESS GREAT PERIOD COMMA COLON SEMICOLON ARROW
%token <Syntax.pos_info> EOF

%right ARROW
%left BINOP1L
%left BINOP2L
%right BINOP3R
%right BINOP4R
%left BINOP5L
%right BINOP6R
%left ASTE

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
  | LET rec_flag = option(REC) name = VAR arguments = list(argument) EQUAL t = term
  {
    LetDec((match rec_flag with Some _ -> true | _ -> false),fst name,arguments,t,$1)
  }
  | TYPEDEF name = CONS typevars = type_argument EQUAL typedec = separated_list(OR,type_declare)
  {
    TypeDef(fst name,typevars,typedec,$1)
  }

type_argument:
  | VAR
  {
    [fst $1]
  }
  | LPAREN separated_list(COMMA,argument) RPAREN
  {
    $2
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
  | type_expr ARROW type_expr
  {
    $1 @-> $3
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
    TyCon (Tycon (fst $1))
  }

argument:
  | VAR
  {
    fst $1
  }

pattern:
  | simple_pattern
  {
    $1
  }
  | pattern simple_pattern
  {
    App($1,$2)
  }

simple_pattern:
  | LPAREN e = pattern RPAREN
  {
    e
  }
  | LPAREN tuple_list = separated_list(COMMA,pattern) RPAREN
  {
    translate_multi_tuple_to_pair tuple_list $1
  }
  | VAR
  {
    Var(fst $1,"",snd $1)
  }
  | CONS app = option(simple_pattern)
  {
    Cons(fst $1,app,snd $1)
  }

pattern_declare:
  | pattern ARROW term
  {
    ($1,$3)
  }

term:
  | simple_term
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
  | MATCH 
    t = term WITH 
    pattern_dec = separated_list(OR,pattern_declare)
  {
    Match (t,pattern_dec,$1)
  }
  | t1 = term op = BINOP1L t2 = term
  {
    App(App(Var(fst op,"",snd op),t1),t2)
  }
  | t1 = term op = BINOP2L t2 = term
  {
    App(App(Var(fst op,"",snd op),t1),t2)
  }
  | t1 = term op = BINOP3R t2 = term
  {
    App(App(Var(fst op,"",snd op),t1),t2)
  }
  | t1 = term op = BINOP4R t2 = term
  {
    App(App(Var(fst op,"",snd op),t1),t2)
  }
  | t1 = term op = BINOP5L t2 = term
  {
    App(App(Var(fst op,"",snd op),t1),t2)
  }
  | t1 = term op = BINOP6R t2 = term
  {
    App(App(Var(fst op,"",snd op),t1),t2)
  }

simple_term:
  | LPAREN tuple_list = separated_list(COMMA,term) RPAREN
  {
    translate_multi_tuple_to_pair tuple_list $1
  }
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
  | CONS app = option(simple_term)
  {
    Cons(fst $1,app,snd $1)
  }