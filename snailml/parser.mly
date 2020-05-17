%{
  open Typedef
  open Syntax

  let translate_multi_tuple_to_pair tup_list pos =
    match tup_list with
      [] -> Cons (",",None,pos)
    | x :: xs -> List.fold_left (fun acc -> fun a -> Prod(acc,a,pos)) x xs

  let generate_list base_list pos =
    List.fold_right (fun x acc -> Cons("::",Some(Prod(x,acc,pos)),pos)) base_list (Cons("[]",None,pos))

  let make_type_level_pair var_list =
    match var_list with
    | h :: xs -> List.fold_left (fun acc x -> TyPair(acc,x)) h xs
    | [] -> TyCon(Tycon "none")
%}

%token <int*Syntax.pos_info> INT
%token <float*Syntax.pos_info> FLOAT
%token <string*Syntax.pos_info> STRING
%token <string*Syntax.pos_info> CONS VAR BINOP1L BINOP2L BINOP3R BINOP4R BINOP5L BINOP6R
%token <Syntax.pos_info> LPAREN RPAREN LBRAC RBRAC LCBRAC RCBRAC 
%token <Syntax.pos_info> LET FUN IN REC TYPEDEF OF ASTE OR IF THEN ELSE
%token <Syntax.pos_info> MATCH WITH END UNIT AND WILDCARD
%token <Syntax.pos_info> EQUAL PERIOD COMMA COLON SEMICOLON ARROW NILLIST
%token <Syntax.pos_info> EOF

%left COLON
%right ARROW
%right BINOP6R
%left BINOP5L
%right BINOP4R
%right BINOP3R
%left BINOP2L
%left BINOP1L
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
  | LET rec_flag = option(REC) name = let_name arguments = list(argument) type_annot = option(type_annotation) EQUAL t = term mutual_rec = mutual_recursion_top_let
  {
    LetDec((match rec_flag with Some _ -> true | _ -> false),name,arguments,t,type_annot,$1,mutual_rec)
  }
  | TYPEDEF name = CONS typevars = type_argument EQUAL typedec = separated_list(OR,type_declare) mutual_rec = mutual_recursion_top_typedec
  {
    TypeDef(fst name,typevars,typedec,$1,mutual_rec)
  }

mutual_recursion_top_let:
  |
  {
    []
  }
  | AND name = let_name arguments = list(argument) type_annot = option(type_annotation) EQUAL t = term mutual_rec = mutual_recursion_top_let
  {
    LetDec(true,name,arguments,t,type_annot,$1,[]) :: mutual_rec
  }

mutual_recursion_top_typedec:
  |
  {
    []
  }
  | AND name = CONS typevars = type_argument EQUAL option(OR) typedec = separated_list(OR,type_declare) mutual_rec = mutual_recursion_top_typedec
  {
    TypeDef(fst name,typevars,typedec,$1,[]) :: mutual_rec
  }

type_argument:
  | VAR
  {
    [fst $1]
  }
  | LPAREN separated_list(COMMA,type_tuple_argument) RPAREN
  {
    $2
  }
  |
  {
    []
  }

type_tuple_argument:
  | VAR
  {
    fst $1
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

type_annotation:
  | COLON type_expr
  {
    Forall $2
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
  | LPAREN separated_list(COMMA,type_expr) RPAREN
  {
    make_type_level_pair $2
  }
  | UNIT
  {
    TyCon (Tycon "()")
  }

argument:
  | LPAREN VAR typ = type_annotation RPAREN
  {
    (fst $2,Some (typ))
  }
  | VAR
  {
    (fst $1,None)
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
  | simple_pattern binop = bin_op simple_pattern
  {
    Cons(fst binop,Some (Prod($1,$3,snd binop)),snd binop)
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
  | WILDCARD
  {
    Var("___none","",$1)
  }
  | NILLIST
  {
    Cons("[]",None,$1)
  }

pattern_declare:
  | pattern ARROW term
  {
    ($1,$3)
  }

bin_op:
  | BINOP1L
  {
    $1
  }
  | BINOP2L
  {
    $1
  }
  | BINOP3R
  {
    $1
  }
  | BINOP4R
  {
    $1
  }
  | BINOP5L
  {
    $1
  }
  | BINOP6R
  {
    $1
  }

let_name:
  | VAR
  {
    fst $1
  }
  | LPAREN bin_op RPAREN
  {
    fst $2
  }

mutual_recursion_let:
  |
  {
    []
  }
  | AND name = let_name arguments = list(argument) type_annot = option(type_annotation) EQUAL e1 = term mutual_rec = mutual_recursion_let
  {
    MutLetBind(name,"",arguments,e1,type_annot,$1) :: mutual_rec
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
  | LET rec_flag = option(REC) name = let_name arguments = list(argument) type_annot = option(type_annotation) EQUAL e1 = term mutual_rec = mutual_recursion_let IN e2 = term
  {
    Let((match rec_flag with Some _ -> true | _ -> false),name,"",arguments,e1,e2,type_annot,$1,mutual_rec)
  }
  | FUN arguments = list(argument) ARROW e = term
  {
    Fun(arguments,"",e,$1)
  }
  | MATCH 
    t = term WITH option(OR)
    pattern_dec = separated_list(OR,pattern_declare)
  {
    Match (t,pattern_dec,$1)
  }
  | IF cond = term THEN then_term = term ELSE else_term = term
  {
    If(cond,then_term,else_term,$1)
  }
  | t1 = term op = ASTE t2 = term
  {
    App(App(Var("*","",op),t1),t2)
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
  | LPAREN e = term type_annot = option(type_annotation) RPAREN
  {
    match type_annot with
    | Some x -> TypeAnnot(e,x)
    | None -> e
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
  | UNIT
  {
    Cons("()",None,$1)
  }
  | NILLIST
  {
    Cons("[]",None,$1)
  }
  | LBRAC base_list = separated_list(COMMA,term) RBRAC
  {
    generate_list base_list $1
  }