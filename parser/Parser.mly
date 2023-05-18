%{
  open Ast

  (* Le parseur doit générer des Ast.program. *)
  (* Pour les annotations, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation. *)
%}
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> ID
%token <bool> BOOL

%token EOF
%token SEMICOLON
%token  L_SQ_BRK 
%token  R_SQ_BRK 
%token  L_PAR 
%token  R_PAR 
%token DOT
%token COMMA
%token PI
%token ADD SUB MUL DIV MOD
%token EQ NE LT GT LE GE
%token CONS
%token AND OR NOT
%token HEAD TAIL FLOOR COS SIN BOOL_TYPE FROM IN
%token PRINT DRAW ASSIGNMENT
%token BEGIN END IF ELSE FOR TO STEP FOREACH
%token INT_TYPE FLOAT_TYPE FLOAT_OF_INT POSITION COLOR POINT LIST
%token X_ACCESSOR Y_ACCESSOR RED_ACCESSOR GREEN_ACCESSOR BLUE_ACCESSOR
%token TRUE FALSE








%left AND OR
%left EQ NE LT GT LE GE
%left ADD 
%left SUB
%left MUL DIV MOD
%left CONS DOT 

//%right ELSE






%nonassoc NOT
//%nonassoc LOWER_THAN_ELSE



//%left TAIL SIN POSITION POINT PI L_SQ_BRK L_PAR INT_TYPE ID HEAD FLOOR FLOAT_TYPE FLOAT_OF_INT COS COLOR BOOL_TYPE
/*
%left END
%left IF FOREACH DRAW PRINT FOR ELSE
%left FROM STEP
%left COLOR POINT POS
%left AND OR
%left EQ NEQ LT GT LEQ GEQ
%left ADD SUB
%left MUL DIV MOD
%left COS SIN HEAD TAIL FLOOR FLOAT_OF_INT
%left GREEN_ACCESSOR RED_ACCESSOR BLUE_ACCESSOR X_ACCESSOR Y_ACCESSOR POSITION
%left BEGIN
%nonassoc NOT
%nonassoc USUB
%left COMMA DOT L_BRK L_PAR R_BRK R_PAR*/

%start <program> main
%%

main:
| LT arg=argument_list GT stmts=statement_list EOF { Program(arg, Block( stmts, Annotation.create $loc)) }
| stmts= statement_list EOF { Program([], Block(stmts, Annotation.create $loc)) }
//%prec LOWER_THAN_ELSE



/*
if_else_suffix:
  | ELSE statement { $2 }
  |  { Nop }
*/



statement:
| type_e = type_expr L_PAR name = ID R_PAR  { Variable_declaration(name,type_e, Annotation.create $loc)}
| BEGIN list= statement_list END  { Block(list, Annotation.create $loc) }
| IF L_PAR e=expression R_PAR i=statement  { IfThenElse(e, i, Nop, Annotation.create $loc) }
| IF L_PAR e=expression R_PAR i1=statement ELSE i2=statement  { IfThenElse(e, i1, i2, Annotation.create $loc) }

| FOR name=ID FROM ei=expression TO et=expression STEP es=expression body=statement  { For(name, ei, et, es, body, Annotation.create $loc) }
| FOREACH name=ID IN e=expression body=statement  { Foreach(name, e, body, Annotation.create $loc) } 
| DRAW expression  { Draw($2, Annotation.create $loc) }
| PRINT expression  { Print($2, Annotation.create $loc) }
//| ASSIGNMENT e1=expression e2=expression SEMICOLON { Assignment(e1, e2, Annotation.create $loc) }
| ASSIGNMENT  L_PAR e1=expression COMMA e2=expression R_PAR  { Assignment(e1, e2, Annotation.create $loc) }

statement_list:
| t = statement_list SEMICOLON statement = statement { t@[statement] }
| statement = statement { [statement] }
| { [] }


argument:
| typ=type_expr L_PAR name=ID R_PAR { Argument (name, typ, Annotation.create $loc) }


argument_list:
| t = argument_list SEMICOLON arg = argument { t@[arg] }
| arg = argument { [arg] }
| { [] }

expression_list:
| expr = expression { [expr] }
| t2 = expression_list COMMA expr = expression { t2@[expr] }
| { [] }



expression:


| int_= INT { Constant_i(int_, Annotation.create $loc) }
| float_=FLOAT { Constant_f(float_, Annotation.create $loc) }
| bool_=BOOL { Constant_b(bool_, Annotation.create $loc) }
//| LIST L_PAR INT_TYPE R_PAR L_PAR ID R_PAR { List([args],Annotation.create $loc)}








/*
//| FLOAT_OF_INT L_PAR int_=INT R_PAR SEMICOLON { Constant_f((float) int_, Annotation.create $loc) } */



| PI  { Constant_f(Float.pi, Annotation.create $loc) }
| BOOL_TYPE L_PAR bool_=BOOL R_PAR SEMICOLON  { Constant_b(bool_, Annotation.create $loc) }
| TRUE  { Constant_b(true, Annotation.create $loc) }
| FALSE  { Constant_b(false, Annotation.create $loc) }
| ID { Variable($1, Annotation.create $loc) } 
| POSITION L_PAR e1=expression COMMA e2=expression R_PAR { Pos(e1,e2, Annotation.create $loc) }
| COLOR L_PAR e1=expression COMMA e2=expression COMMA e3=expression R_PAR { Color(e1, e2, e3, Annotation.create $loc)}
| POINT L_PAR e1=expression COMMA e2=expression R_PAR { Point(e1, e2, Annotation.create $loc)}
| e1 = expression b = binop e2 =expression %prec SUB { Binary_operator(b, e1, e2, Annotation.create $loc)}
| u= unop e = expression { Unary_operator(u,e, Annotation.create $loc) } %prec NOT
| e= expression DOT f=field { Field_accessor(f,e, Annotation.create $loc) }
| L_SQ_BRK args = expression_list R_SQ_BRK { List(List.rev args, Annotation.create $loc) }
//| L_SQ_BRK  args = expression R_SQ_BRK { List([args],Annotation.create $loc)}
| e1 = expression CONS e2 =expression { Cons( e1, e2, Annotation.create $loc)}
| L_PAR e=expression R_PAR { e }
    

   
//| L_PAR e1=expression COMMA e2=expression R_PAR { Tuple(e1,e2, Annotation.create $loc) }
/*
| HEAD L_PAR e=expression R_PAR { Head(e, Annotation.create $loc) }
| TAIL L_PAR e=expression R_PAR { Tail(e, Annotation.create $loc) }
| FLOOR L_PAR e=expression R_PAR { Floor(e, Annotation.create $loc) }
| FLOAT_OF_INT L_PAR e=expression R_PAR { Float_of_int(e, Annotation.create $loc) }
| COS L_PAR e=expression R_PAR { Cos(e, Annotation.create $loc) }
| SIN L_PAR e=expression R_PAR { Sin(e, Annotation.create $loc) }
| GREEN_ACCESSOR L_PAR e=expression R_PAR { Green(e, Annotation.create $loc) }
| RED_ACCESSOR L_PAR e=expression R_PAR { Red(e, Annotation.create $loc) }
| BLUE_ACCESSOR L_PAR e=expression R_PAR { Blue(e, Annotation.create $loc) }
| X_ACCESSOR L_PAR e=expression R_PAR { X(e, Annotation.create $loc) }
| Y_ACCESSOR L_PAR e=expression R_PAR { Y(e, Annotation.create $loc) }
| 
*/



type_expr:
| INT_TYPE        { Type_int }
| FLOAT_TYPE      { Type_float }
| BOOL_TYPE       { Type_bool }
| POSITION        { Type_pos }
| COLOR           { Type_color }
| POINT           { Type_point }
| LIST L_PAR typ=type_expr R_PAR { Type_list(typ) } 
//| LIST L_PAR t = type_expr R_PAR { Type_list(t) }



binop:
| ADD             { Add }
| SUB             { Sub }
| MUL             { Mul }
| DIV             { Div }
| MOD             { Mod }
| AND             { And }
| OR              { Or }
| EQ              { Eq }
| NE              { Ne }
| LT              { Lt }
| GT              { Gt }
| LE              { Le }
| GE              { Ge }

%inline unop:
| SUB             { USub } 
| NOT             { Not }
| HEAD            { Head }
| TAIL            { Tail }
| FLOOR           { Floor }
| FLOAT_OF_INT    { Float_of_int }
| COS             { Cos }
| SIN             { Sin }


%inline field:
| COLOR           { Color_accessor}
| POSITION        { Position_accessor }  
| X_ACCESSOR      { X_accessor }
| Y_ACCESSOR      { Y_accessor }
| RED_ACCESSOR    { Red_accessor }
| GREEN_ACCESSOR  { Green_accessor }
| BLUE_ACCESSOR   { Blue_accessor }


