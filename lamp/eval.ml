open Ast

let todo () = failwith "TODO"

exception Stuck of string
(** Exception indicating that evaluation is stuck *)

(** Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(** Computes the set of free variables in the given expression *)
let rec free_vars (e : expr) : Vars.t =
    (* This line imports the functions in Vars, so you can write [diff .. ..]
       instead of [Vars.diff .. ..] *)
    let open Vars in
    (* Your code goes here *)
    match e with
    | Num _ -> empty
    | Binop (_, lhs, rhs) -> union (free_vars lhs) (free_vars rhs)
    | Var x -> singleton x
    | Lambda (ty, (param, body)) -> diff (free_vars body) (singleton param)
    | App (fn, arg) -> union (free_vars fn) (free_vars arg)
    | Let (value, (name, expr)) -> union (free_vars value) (diff (free_vars expr) (singleton name))
    | True -> empty
    | False -> empty 
    | IfThenElse (cond, tt, ff) -> union (union (free_vars cond) (free_vars tt)) (free_vars ff) 
    | Comp (_, lhs, rhs) -> union (free_vars lhs) (free_vars rhs)  
    | ListNil ty -> empty 
    | ListCons (head, tail) -> union (free_vars head) (free_vars tail) 
    | ListMatch (scrutinee, nil_case, (h, (t, cons_case))) ->
        let scrutinee_vars = free_vars scrutinee in
        let nil_vars = free_vars nil_case in
        let cons_vars = diff (diff (free_vars cons_case) (singleton h)) (singleton t) in 
        union (union scrutinee_vars nil_vars) cons_vars 
    | Fix (ty, (self, body)) -> diff (free_vars body) (singleton self) 
    | _ -> im_stuck (Fmt.str "Unknown expression type: %a" Pretty.expr e)

(** Perform substitution c[x -> e], i.e., substituting x with e in c *)
let rec subst (x : string) (e : expr) (c : expr) : expr =
    match c with
    | Num n -> Num n
    | Binop (binop, lhs, rhs) -> Binop (binop, subst x e lhs, subst x e rhs)
    | Var y -> if x = y then e else Var y
    | Lambda (ty, (param, body)) ->
        let body' = 
            if String.equal x param || Vars.mem param (free_vars e)
            then body 
            else subst x e body in
        Lambda (ty, (param, body'))
    | App (fn, arg) -> App (subst x e fn, subst x e arg)
    | Let (value, (name, body)) ->
        let body' = 
            if String.equal x name || Vars.mem name (free_vars e) 
            then body 
            else subst x e body in 
        Let (subst x e value, (name, body'))
    | True -> True 
    | False -> False 
    | IfThenElse (cond, tt, ff) -> IfThenElse (subst x e cond, subst x e tt, subst x e ff) 
    | Comp (relop, lhs, rhs) -> Comp (relop, subst x e lhs, subst x e rhs) 
    | ListNil ty -> ListNil ty 
    | ListCons (head, tail) -> ListCons (subst x e head, subst x e tail) 
    | ListMatch (scrutinee, nil_case, (h, (t, cons_case))) -> 
        let scrutinee' = subst x e scrutinee in
        let nil_case' = subst x e nil_case in
        let cons_case' = 
            if String.equal x h || String.equal x t
            then cons_case 
            else subst x e cons_case 
        in
        ListMatch (scrutinee', nil_case', (h, (t, cons_case')))
    | Fix (self, body) ->
        let body' = if String.equal x self then body else subst x e body in 
        Fix (self, body')    
    | _ -> im_stuck (Fmt.str "Unknown expression type: %a" Pretty.expr e)

(** Perform substitution c[x -> e], i.e., substituting x with e in c *)
let rec subst (x : string) (e : expr) (c : expr) : expr =
  match c with _ -> todo ()

(** Evaluate expression e *)
let rec eval (e : expr) : expr =
  try match e with _ -> todo ()
  with Stuck msg ->
    im_stuck (Fmt.str "%s\nin expression %a" msg Pretty.expr e)
