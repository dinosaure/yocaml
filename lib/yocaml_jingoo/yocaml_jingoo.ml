module Renderable = struct
  open Jingoo

  type t = Jg_types.tvalue

  let object_ list = Jg_types.Tobj list
  let list list = Jg_types.Tlist list
  let string str = Jg_types.Tstr str
  let boolean b = Jg_types.Tbool b
  let integer i = Jg_types.Tint i
  let float f = Jg_types.Tfloat f
  let atom s = Jg_types.Tstr s
  let null = Jg_types.Tnull

  let track_dependencies ~env ~models ast =
    let dependencies = ref [] in
    let rec go env ctx = function
      | Jingoo.Jg_types.IncludeStatement (e, _with_ctx) ->
        (match Jingoo.Jg_interp.value_of_expr env ctx e with
         | Tstr path | Tsafe path ->
           dependencies := path :: !dependencies;
           let ast = Jingoo.Jg_interp.ast_from_file ~env path in
           List.fold_left (go env) ctx ast
         | _ -> assert false)
      | expr -> Jingoo.Jg_interp.eval_statement env ctx expr
    in
    let ctx = Jingoo.Jg_interp.init_context ~env ~models ~output:ignore () in
    let _ = List.fold_left (go env) ctx ast in
    !dependencies
  ;;

  let dependencies ?(strict = true) variables tpl =
    let env = Jg_types.{ std_env with strict_mode = strict } in
    let env, ast, _macros = Jg_interp.Loaded.from_string ~env tpl in
    let models key = List.assoc key variables in
    track_dependencies ~env ~models ast
  ;;

  let to_string ?(strict = true) variables tpl =
    let env = Jg_types.{ std_env with strict_mode = strict } in
    Jg_template.from_string ~env ~models:variables tpl
  ;;
end

include Renderable

let apply_as_template
  (type a)
  (module I : Yocaml.Metadata.INJECTABLE with type t = a)
  ?(strict = true)
  template
  =
  Yocaml.Build.apply_as_template
    (module I)
    (module Renderable)
    ~strict
    template
;;
