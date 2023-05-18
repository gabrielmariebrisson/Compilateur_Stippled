open Ast


(* Codez ici le simplificateur de termes.

    Tout comme pour le langage du cours, l’idée consiste à remplacer les termes constants par le résultat de leur calcul.

    Faites une sous-fonctions récursive pour les expressions et les statements.
    Ces fonction font un pattern matching sur leur argument et traitent chaque cas séparément. Elles renvoient un argument de même type que celui reçu.
    Par exemple : simplifier_expression : Ast.expression -> Ast.expression

    Les cas minimaux attendus sont les cas sur les entiers, les flottants, les booléens, ainsi que les if dont le test est constant, et les for qui ne s’exécutent jamais.

    Deux points qui peuvent vous permettre d’aller plus loin :
      - les expressions ne peuvent pas faire d’effet de bord ici, ce qui permet de simplifier des expressions pas nécessairement constantes.
      - Les types composés (point, position et couleur) peuvent également être simplifiés (e.g., (1,2) + (2,x) peut être simplifié en (3,2+x)).

    Vous détaillerez dans le rapport les différents cas que vous simplifiez dans votre simplificateur.
*)

let rec simplifier_expr expr =
  match expr with
  | Binary_operator (op, e1, e2, annotation) -> (
      let e1 = simplifier_expr e1 in
      let e2 = simplifier_expr e2 in
        match (op, e1, e2) with
        | Add, Constant_i (i1, _), Constant_i (i2, _) -> Constant_i (i1 + i2, annotation)
        | Sub, Constant_i (i1, _), Constant_i (i2, _) -> Constant_i (i1 - i2, annotation)
        | Mul, Constant_i (i1, _), Constant_i (i2, _) -> Constant_i (i1 * i2, annotation)
        | Div, Constant_i (i1, _), Constant_i (i2, _) -> Constant_i (i1 / i2, annotation)
        | Mod, Constant_i (i1, _), Constant_i (i2, _) -> Constant_i (i1 mod i2, annotation)
        | Add, Constant_f (f1, _), Constant_f (f2, _) -> Constant_f (f1 +. f2, annotation)
        | Sub, Constant_f (f1, _), Constant_f (f2, _) -> Constant_f (f1 -. f2, annotation)
        | Mul, Constant_f (f1, _), Constant_f (f2, _) -> Constant_f (f1 *. f2, annotation)
        | Div, Constant_f (f1, _), Constant_f (f2, _) -> Constant_f (f1 /. f2, annotation)
        | Mod, Constant_f (i1, _), Constant_f (i2, _) -> Constant_f (mod_float i1 i2, annotation)
        | And, Constant_b (b1, _), Constant_b (b2, _) -> Constant_b (b1 && b2, annotation)
        | Or, Constant_b (b1, _), Constant_b (b2, _) -> Constant_b (b1 || b2, annotation)
        | Eq, Constant_b (b1, _), Constant_b (b2, _) -> Constant_b (b1 = b2, annotation)
        | Eq, Constant_i (b1, _), Constant_i (b2, _) -> Constant_b (b1 = b2, annotation) 
        | Eq, Constant_f (b1, _), Constant_f (b2, _) -> Constant_b (b1 = b2, annotation)
        | Ne, Constant_b (b1, _), Constant_b (b2, _) -> Constant_b (b1 <> b2, annotation)
        | Ne, Constant_i (b1, _), Constant_i (b2, _) -> Constant_b (b1 <> b2, annotation) 
        | Ne, Constant_f (b1, _), Constant_f (b2, _) -> Constant_b (b1 <> b2, annotation)
        | Lt, Constant_b (b1, _), Constant_b (b2, _) -> Constant_b (b1 < b2, annotation)
        | Lt, Constant_i (b1, _), Constant_i (b2, _) -> Constant_b (b1 < b2, annotation) 
        | Lt, Constant_f (b1, _), Constant_f (b2, _) -> Constant_b (b1 < b2, annotation)
        | Gt, Constant_b (b1, _), Constant_b (b2, _) -> Constant_b (b1 > b2, annotation)
        | Gt, Constant_i (b1, _), Constant_i (b2, _) -> Constant_b (b1 > b2, annotation) 
        | Gt, Constant_f (b1, _), Constant_f (b2, _) -> Constant_b (b1 > b2, annotation)
        | Le, Constant_b (b1, _), Constant_b (b2, _) -> Constant_b (b1 <= b2, annotation)
        | Le, Constant_i (b1, _), Constant_i (b2, _) -> Constant_b (b1 <= b2, annotation) 
        | Le, Constant_f (b1, _), Constant_f (b2, _) -> Constant_b (b1 <= b2, annotation)
        | Ge, Constant_b (b1, _), Constant_b (b2, _) -> Constant_b (b1 >= b2, annotation)
        | Ge, Constant_i (b1, _), Constant_i (b2, _) -> Constant_b (b1 >= b2, annotation)
        | Ge, Constant_f (b1, _), Constant_f (b2, _) -> Constant_b (b1 >= b2, annotation)
        | _ -> Binary_operator (op, e1, e2, annotation))
  | Unary_operator (op, e, annotation) -> (
      let e = simplifier_expr e in
        match (op, e) with
        | USub, Constant_i (i, _) -> Constant_i (-i, annotation)
        | USub, Constant_f (f, _) -> Constant_f (-.f, annotation)
        | Not, Constant_b (b, _) -> Constant_b (not b, annotation)
        | _ -> Unary_operator (op, e, annotation))
  | Pos (e1, e2, annotation)-> Pos (simplifier_expr e1, simplifier_expr e2, annotation)
  | Point (e1, e2, annotation)-> Point (simplifier_expr e1, e2, annotation)
  | Field_accessor (acc, e, annotation)->Field_accessor (acc, simplifier_expr e, annotation)
  | _ -> expr

let rec simplifier_statement statement =
  match statement with
  | Assignment (mutable_expression, expression, annotation) -> Assignment (mutable_expression, simplifier_expr expression, annotation)
  | Block (list_statements, annotation) -> Block (List.map simplifier_statement list_statements, annotation)
  | IfThenElse (test, statement_then, statement_else, annotation) -> (
    let test = simplifier_expr test in
    match test with
      | Constant_b (true, _) -> simplifier_statement statement_then
      | Constant_b (false, _) -> simplifier_statement statement_else
      | _ ->IfThenElse ( test, simplifier_statement statement_then, simplifier_statement statement_else, annotation ) )
  | For (name, from_expression, to_expression, step_expression, statement, annotation) -> For( name, simplifier_expr from_expression, simplifier_expr to_expression, simplifier_expr step_expression, simplifier_statement statement, annotation )
  | Foreach (name, expression, body, annotation) -> Foreach (name, simplifier_expr expression, simplifier_statement body, annotation)
  | Draw (expression, annotation)-> Draw(simplifier_expr expression, annotation)
  | Print (expression, annotation) -> Print (simplifier_expr expression,annotation)
  | e ->e

let simplifier (Program (args, body)) =
  Program (args, simplifier_statement body)
