module Ctx : module type of Map.Make(String)

type obj =
    { o_type : obj option ref
    ; o_level : int 
    ; self    : string
    ; methods : (string * meth) list }
and value =
    | Obj       of obj
    | Primitive of primitive
and meth =
    { pot   : int
    ; typ   : expression option
    ; value : expression option }
and expression =
    | New       of expression
    | Method    of expression * string
    | Id        of string
    | InhObj    of int * string
                   * [`Inherit of expression 
                     |`Base of (string * meth) list] list
    | Value     of value
and primitive = 
    { compile    : (expression -> Js.expression) ->
                       Js.expression
    ; type_of    : value Ctx.t ->
                   value Ctx.t ->
                       value
    ; evaluate   : value Ctx.t ->
                   value Ctx.t ->
                       value
    ; p_level    : value Ctx.t ->
                       int
    ; subtype_of : value Ctx.t ->
                   value ->
                       bool
    }

type definition = string * expression
type program = definition list * expression

val string_of_expression : expression -> string
