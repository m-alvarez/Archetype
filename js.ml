open Reporting 

type id = string
type literal =
    | Obj  of (id * method_decl) list
    | Fn   of id list * expression
    | Int  of int
    | Bool of bool
    | Prim of string
and method_decl = 
    { self : id option
    ; pot  : int
    ; body : expression }
and expression =
    | Lit of literal
    | Name of id
    | New of expression
    | Extend of expression * expression
    | Apply of expression * expression list
    | Method of expression * id
    | Add  of expression * expression
    | Sub  of expression * expression
    | Eq   of expression * expression
    | If   of expression * expression * expression

module Context = Set.Make(String)


let rec compile context = function
    | Lit(l)  -> compile_literal context l
    | Name(n) -> 
            if Context.mem n context
            then n
            else raise (Failure (fmt "Unknown name %s" n))
    | New(e) ->
            "instantiate(" ^ compile context e ^ ")"
    | Extend(e1, e2) ->
            "extend(" ^ compile context e1 ^ "," ^ compile context e2 ^ ")"
    | Apply(e1, exprs) ->
            let args = List.map (compile context) exprs in
            let arg_str = String.concat "," args in
            compile context e1 ^ "(" ^ arg_str ^ ")"
    | Method(e, i) ->
            let e = compile context e in
            "invoke(" ^ e ^ ", \"" ^ i ^ "\")"
    | Add(l, r) ->
            let l = compile context l in
            let r = compile context r in
            fmt "(%s + %s)" l r
    | Sub(l, r) ->
            let l = compile context l in
            let r = compile context r in
            fmt "(%s - %s)" l r
    | Eq(l, r) ->
            let l = compile context l in
            let r = compile context r in
            fmt "(%s === %s)" l r
    | If(cond, thn, els) ->
            let cond = compile context cond in
            let thn  = compile context thn  in
            let els  = compile context els  in
            fmt "(%s)?(%s):(%s))" cond thn els
and compile_literal context = function
    | Obj(methods) -> 
            let methods = List.map (compile_method context) methods in
            "{" ^ String.concat "," methods ^ "}"
    | Fn(args, body) ->
            let new_context = List.fold_right Context.add args context in
            Format.sprintf
            "function( %s ) {
                var result = %s;
                return result;
             }" (String.concat "," args) (compile new_context body)
    | Int(i) ->
            string_of_int i
    | Bool(true) ->
            "true"
    | Bool(false) ->
            "false"
    | Prim(s) ->
            fmt "(%s)" s
and compile_method context (name, decl) =
    Format.sprintf "%s : %s" name (compile_method_body context decl)
and compile_method_body context decl =
    Format.sprintf "[%d, function() {
        %s
        return %s;
    }]" decl.pot 
        (match decl.self with
        | None -> ""
        | Some(self) -> "var " ^ self ^ "=this;")
        (compile (match decl.self with
                 | None -> context
                 | Some(self) -> Context.add self context) decl.body)
 
let add_prelude s =
    Format.sprintf
    "(function() {
        function invoke(obj, method) {
            var m = obj[method];
            var result;
            if (m[0] == 0) {
                return m[1].apply(obj);
            } else {
                throw Error(\"Method \" + method +
                    \" of object \" + obj + \" has potency \" + m[0]);
            }
        }
        function extend(obj1, obj2) {
            var object = {};
            for(method in obj1) {
                object[method] = obj1[method];
            }
            for(method in obj2) {
                object[method] = obj2[method];
            }

            return object;
        }
        function instantiate(obj) {
            var object = {};
            for(method in obj) {
                var potency = obj[method][0];
                var fn = obj[method][1];
                if(potency > 0) {
                    object[method] = [potency - 1, fn];
                }
            }
            return object;
        }
        return %s
     })()" s

let compile_program expr =
    add_prelude (compile Context.empty expr)

