open Ast
open Reporting

let current_method_pot = ref 0

let print_context c =
    print_string "Context: \n";
    Ctx.iter (fun k v ->
        print_string @@ fmt "\t%s\n" k)
        c

let lookup l name =
    try
        Some (List.assoc name l)
    with
    | Not_found -> None

let lookup_name c i =
    try
        Ctx.find i c
    with Not_found ->
        raise (No_such_name i)

let lookup_method obj m =
    try
        List.assoc m obj.methods
    with Not_found ->
        raise (No_such_method m)

let as_obj e =
    match e with
    | Obj o -> o
    | Primitive _ -> compiler_error "Expected object, got primitive instead"

let value_level values v =
    match v with
    | Obj o -> o.o_level
    | Primitive p -> p.p_level values

let check_method_level values level name typ =
    if value_level values typ < 1
    then compiler_error @@ fmt "Runtime object used as inferred type for method %s" name;

    if value_level values typ > level + 1
    then compiler_error @@ fmt "Method %s escapes layer boundaries" name

let update_method_tables t_meths c_meths name meth =
    c_meths := (name, meth) :: !c_meths;
    t_meths := (name, { meth with pot = meth.pot + 1; value = None }) :: !t_meths

let methods_from_sections (sections : int) = 
    ()

let rec type_of_method_list types values self level methods =
    let values  = ref values      in
    let types   = ref types       in
    let c_meths = ref []          in
    let t_meths = ref []          in
    List.map (function name,meth ->
        let o_typ = { self; o_type = ref None; o_level = level + 1; methods = !t_meths} in
        values := Ctx.add self (Obj{self;o_type = ref (Some o_typ); o_level = level; methods = !c_meths}) !values;
        types  := Ctx.add self (Obj o_typ) !types; 

        if meth.pot > level
        then type_error @@ fmt "Method %s will never be instantiated" name;
        current_method_pot := meth.pot;
        let t = 
            match meth.typ, meth.value with
            | None, None ->
                type_error @@ fmt "Untyped abstract value %s" name
            | Some t, None ->
                if level = 0
                then type_error @@ fmt "Runtime object with abstract method %s" name;
                evaluate !types !values t
            | None, Some(e) ->
                type_of !types !values e
            | Some(t), Some(e) ->
                let t' = type_of !types !values e  in
                let t  = evaluate !types !values t in
                let t'_level = value_level !values t' in
                let t_level  = value_level !values t  in
                if t_level <> t'_level
                then type_error @@ fmt "Level mismatch on method %s: has %d, expected %d" name t'_level t_level;
                if not (subtype_of !types !values t' t)
                then type_error @@ fmt "Type mismatch on method %s" name;
                t
        in
        check_method_level !values level name t;
        update_method_tables t_meths c_meths name { meth with typ = Some (Value t) };
        name, { pot = meth.pot + 1; typ = Some (Value t); value = None }
    ) methods


and type_of_value types values exp =
    match exp with
    | Obj obj ->
        begin
            match ! (obj.o_type) with
            | Some t -> Obj t
            | None ->
                let o_level = obj.o_level + 1 in
                let methods = obj.methods     in

                let methods = type_of_method_list types values obj.self obj.o_level methods in
                let typ = { o_type = ref None; o_level; methods; self = obj.self } in
                obj.o_type := Some typ;
                Obj typ
            end
    | Primitive p ->
        p.type_of types values


and type_of types values exp : value =
    match exp with
    | New exp -> 
        (* little trick to infer more stuff *)
        let t_exp = as_obj @@ type_of types values exp in
        let exp   = as_obj @@ evaluate types values exp      in
        if t_exp.o_level = 0 || exp.o_level = 0
        then compiler_error "Evaluated runtime expression";
        if List.exists (function _,meth ->
            meth.pot < 2 && meth.value = None) 
            exp.methods
        then type_error @@ fmt "Abstract method cannot be instantiated";
        Obj { self = exp.self
            ; o_type = ref None
            ; o_level = exp.o_level
            ; methods =
                List.append
                    (List.map (function n,m -> n, {m with pot = m.pot - 1 }) 
                        (List.filter (function _,m -> m.pot > 0) t_exp.methods))
                    [("class",
                        { pot = 1   
                        ; typ = Some (Value (type_of_value types values (Obj exp)))
                        ; value = Some (Value (Obj exp))})]
            }
    | Method (exp,name) ->
        begin
            let t_exp = as_obj @@ type_of types values exp in
            let meth  = lookup_method t_exp name in
            if meth.pot <> 1 && exp <> Id "self" 
            then type_error @@ fmt "Invoking method %s of potency %d" name (meth.pot - 1);
            if exp = Id "self" && meth.pot - 1  <> !current_method_pot
            then type_error @@ fmt "Invoking method self.%s of potency %d from level %d" 
                name 
                (meth.pot - 1) 
                !current_method_pot;
            match meth.typ with
            | None ->
                compiler_error @@ fmt "Wasn't able to infer the type of method %s" name
            | Some(t) -> 
                evaluate types values t
        end
    | Id id ->
        begin
            try
                Ctx.find id types
            with Not_found ->
                type_of types values @@ Value (lookup_name values id)
        end
    | Value v ->
        type_of_value types values v
    | InhObj (level, self, sections) ->
        compiler_error @@ "Trying to take type of inherited object"


and evaluate types values exp : value =
    match exp with
    | New exp ->
        let exp = as_obj @@ evaluate types values exp in
        if exp.o_level < 1
        then compiler_error @@ "Trying to instantiate a runtime object";
        let o_level = exp.o_level - 1 in
        let methods = List.append (List.map (function name,meth ->
            name, { meth with pot = meth.pot - 1 }
        ) (List.filter (function name,meth ->
            if meth.value = None
            then type_error @@ fmt "Trying to instantiate object with abstract method %s" name;
            if name = "class" && meth.pot > 0
            then type_error @@ fmt "Something wrong happened: class pointer at potency %d" meth.pot;
            meth.pot > 0
        ) exp.methods))
        [("class",
            {pot = 0
            ; typ = Some (Value (type_of_value types values (Obj exp)))
            ; value = Some (Value (Obj exp))})]
        in
        Obj { self = exp.self; o_type = ref (Some exp); o_level; methods }
    | Method (exp, name) ->
        begin
            let exp = as_obj @@ evaluate types values exp in
            if exp.o_level < 1
            then compiler_error @@ "Trying to evaluate a runtime method invocation";
            let meth = lookup_method exp name in
            if meth.pot <> 0
            then type_error @@ fmt "Trying to invoke uninstantiated method %s" name;
            match meth.value with
            | None    ->
                type_error @@ fmt "Trying to invoke abstract method %s" name;
            | Some(b) ->
                let values = Ctx.add exp.self (Obj exp) values in
                let b = evaluate types values b in
                let b_level = value_level values b in
                if b_level < 1
                then compiler_error @@ "Evaluated a runtime expression";
                b
        end
    | Id id ->
        lookup_name values id
    | Value (Obj o) ->
        let _t = type_of types values exp in
        Obj o
    | Value (Primitive p) ->
        p.evaluate types values
    | InhObj (level, self, sections) ->
        compiler_error @@ "Trying to take type of inherited object"


and subtype_of types values t1 t2 =
    let t1_level = value_level values t1 in
    let t2_level = value_level values t2 in
    t1_level = t2_level
    && match t1, t2 with
    | Obj t1, Obj t2 ->
        List.for_all (function name, meth ->
            try
                begin
                    let meth' = lookup_method t1 name in
                    if meth'.pot <> meth.pot
                    then type_error @@ fmt "Potency mismatch for method %s" name;
                    match meth'.typ, meth.typ with
                    | Some(t'), Some(t) ->
                        subtype_of types values (evaluate types values t') (evaluate types values t)
                    | _ ->
                        compiler_error @@ fmt "Failed inference for method %s" name
                end
            with _ -> 
                type_error @@ fmt "Subclass fails to provide method %s" name
        ) (List.filter (function _name,m -> m.pot > 0) t2.methods)
    | Primitive p1, _ ->
        p1.subtype_of values t2
    | _ ->
        false

