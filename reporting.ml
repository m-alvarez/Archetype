exception No_such_method of string
exception No_such_name of string
exception Not_yet_implemented
exception Compiler_error of string
exception Type_error of string

let fmt = Format.sprintf

let compiler_error str =
    raise (Compiler_error str)
let type_error str =
    raise (Type_error str)

let warn str =
    print_endline ("Warning: " ^ str)
