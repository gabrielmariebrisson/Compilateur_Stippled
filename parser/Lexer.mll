
{
    open Parser
    exception Error of string
}

let digit = ['0'-'9'] 
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']

rule token = parse
    | "//" [^ '\n']* '\n' {Lexing.new_line lexbuf; token lexbuf}
    | "/*"              {commentary lexbuf}
    | [' ' '\t' '\r']   {token lexbuf}
    | '\n'              { Lexing.new_line lexbuf ; token lexbuf }

    (*Opérateurs*)
    | "+"               { ADD }
    | "-"               { SUB } 
    | "*"               { MUL }
    | "/"               { DIV }
    | "%"               { MOD }
    | "="               { EQ }
    | "<>"              { NE }
    | "<"               { LT }
    | ">"               { GT }
    | "<="              { LE }
    | ">="              { GE }
    | "::"              { CONS }   (* sous élément de List *)
    | "."               { DOT }
    | "And"             { AND }
    | "Or"              { OR }
    | "Not"             { NOT }
    | "["               { L_SQ_BRK }
    | "]"               { R_SQ_BRK }
    | "("               { L_PAR }
    | ")"               { R_PAR }
    | ","               { COMMA }
    | ";"               { SEMICOLON }
    

    (* Mots clé   *)
    | "Head"            { HEAD }   (* sous élément de List *)
    | "Tail"            { TAIL }   (* sous élément de List *)
    | "Floor"           { FLOOR }
    | "Cos"             { COS }
    | "Sin"             { SIN }
    | "Bool"            { BOOL_TYPE } 
    | "From"            { FROM } 
    | "In"              { IN }
    | "Pi"              { PI }


    (* Instructions *)
    | "Print"           { PRINT }
    | "Draw"            { DRAW }  
    | "Copy"            { ASSIGNMENT }  
    | "Begin"           { BEGIN }
    | "End"             { END } 
    | "If"              { IF } 
    | "Else"            { ELSE }
    | "For"             { FOR } 
    | "To"              { TO } 
    | "Step"            { STEP }
    | "Foreach"         { FOREACH } 

    (* Données   *)
    | "Int"             { INT_TYPE } 
    | "Float_of_int"    { FLOAT_OF_INT }
    | "Float"           { FLOAT_TYPE } 
    | "Pos"             { POSITION }     (* également des Champs*)
    | "Color"           { COLOR }   (* également des Champs*)
    | "Point"           { POINT } 
    | "List"            { LIST }

    (* Champs   *)
    | "X"               { X_ACCESSOR } 
    | "Y"               { Y_ACCESSOR }
    | "Red"             { RED_ACCESSOR }
    | "Green"           { GREEN_ACCESSOR } 
    | "Blue"            { BLUE_ACCESSOR }
    | "True"            { TRUE } 
    | "False"            { FALSE } 

    (* Identificateurs   *)
    (* Je ne sais pas si c'est compris dans le lexer
    | "x"           { X } 
    | "i"           { START }
    | "premier_nombre"           { START }
    | "value12"           { START } *)

    | "\"" ([^ '\"']* as s) "\""  { STRING(s) }
    | (digit)* "." (digit)* as s {FLOAT(try float_of_string s with Failure _ -> raise (Error(s)) )}
    | (digit)+ as s     { INT(try int_of_string s with Failure _ ->(let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%s'. It is not a valid integer" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) ))}
    | eof               { EOF }
    | ['a'-'z' 'A'-'Z'] (alphanum)* as s  { ID(s) }
    | _ as s            { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) }

and commentary = parse
    | '\n'      {Lexing.new_line lexbuf; commentary lexbuf}
    | "*/"      { token lexbuf }
    | _ { commentary lexbuf }