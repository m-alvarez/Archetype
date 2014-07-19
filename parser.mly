%{
    open Ast
%}

%token <string> Id
%token <int> Int
%token <bool> Bool

%token OBJECT

%token INT_TYPE BOOL_TYPE

%token NEW IS INHERIT METHOD INST
%token BEGIN END
%token LBRACE RBRACE LPAR RPAR LBRACKET RBRACKET
%token COMMA SEMICOLON COLON

%token ADD SUB EQ
%token IF THEN ELSE

%token EOF
%token DOT 

%left ADD SUB
%nonassoc EQ

%nonassoc DOT

%nonassoc ELSE

%start pgm
%type <Ast.program> pgm

%%

pgm:
    def_list BEGIN expr END EOF { ($1, $3) }
;

def:
    Id IS expr SEMICOLON { ($1, $3) }
;

def_list: 
    | def def_list { $1 :: $2 }
    | { [] }
;

expr:
    | obj_lit                  { $1 }
    | Id                       { Id($1) }
    | NEW LPAR expr RPAR       { New($3) }
    | LPAR expr RPAR           { $2 }
    | expr DOT Id              { Method($1, $3) }
    | primitive                { Value (Primitive $1) }
    | sugar                    { $1 }
;

num:
    Int { if $1 < 0 then raise (Failure "Number must be positive") else $1 }
;

sugar:
    | new_sugar { $1 }
;

primitive:
    | int_val   { $1 }
    | int_type  { $1 }
    | bool_val  { $1 }
    | bool_type { $1 }
    | if_expr   { $1 }
    | eq        { $1 }
    | bin_op    { $1 }
;

new_sugar:
    | INST opt_pot_decl LPAR expr COMMA new_sugar_args RPAR {
        let pot    = $2 in
        let parent = $4 in
        let meths  = List.map (function n,m -> n,{m with pot = m.pot + 1}) $6 in
        New(
            InhObj (pot, "self",
                [`Base meths; `Inherit parent]
            )
        )
    }
;

new_sugar_args:
    | Id opt_pot_decl IS expr args_tail { 
        ($1, {pot = $2; typ = None; value = Some $4}) :: $5
    }
;

args_tail:
    | { [] }
    | COMMA Id opt_pot_decl IS expr args_tail {
        ($2, {pot = $3; typ = None; value = Some $5}) :: $6
    }
;

if_expr:
    | IF expr THEN expr ELSE expr { Primitives.if_exp $2 $4 $6 }
;

int_val:
    | Int { Primitives.int_lit $1 }
;

int_type:
    | INT_TYPE { Primitives.int_type }
;

bool_val:
    | Bool { Primitives.bool_lit $1 }
;

bool_type:
    | BOOL_TYPE { Primitives.bool_type }
;

eq:
    | expr EQ expr { Primitives.eq $1 $3 }
;

bin_op:
    | expr ADD expr { Primitives.bin_op `Add $1 $3 }
    | expr SUB expr { Primitives.bin_op `Sub $1 $3 }
;

obj_lit:
    OBJECT level_spec self_spec LBRACE
        fields
    RBRACE {
        let level = $2 in
        let self  = "self" in
        match $5 with
        | [`Base methods] ->
            Value (Obj 
                { o_type  = ref None
                ; o_level = level
                ; self    = self 
                ; methods })
        | [] ->
            Value (Obj
                { o_type  = ref None
                ; o_level = level
                ; self    = self
                ; methods = [] })
        | _ ->
            InhObj (level, self, $5)
    }
;

self_spec:
    |  { () }
; 

level_spec:
    | LBRACKET num RBRACKET { $2 }
    | { 0 }
;

fields:
    | { [] }
    | field SEMICOLON fields {
        match $3 with
        | (`Inherit _) :: _
        | [] ->
            `Base [$1] :: $3 
        | (`Base l) :: r ->
            `Base ($1::l) :: r
    }
    | INHERIT expr SEMICOLON fields { (`Inherit $2) :: $4 } 
;

field:
    | METHOD Id opt_pot_decl opt_type_decl field_body {
        ($2,
        { pot   = $3
        ; typ   = $4
        ; value = $5 })
    }
;

opt_pot_decl:
    | { 0 }
    | LBRACKET num RBRACKET { $2 }
;

opt_type_decl:
    |            { None }
    | COLON expr { Some $2 }
;

field_body:
    |         { None }
    | IS expr { Some($2) }
;
