
let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));

open Parsetree;
open Longident;

let js_getter = (target, path) => {
  open Ast_helper;
  path |> List.fold_left((expr, attr: Location.loc(string)) => Exp.apply(
    ~loc=attr.loc,
    Exp.ident(~loc=attr.Location.loc, Location.mkloc(Lident("##"), attr.Location.loc)),
    [("", expr), ("", Exp.ident(~loc=attr.Location.loc, Location.mkloc(Lident(attr.txt), attr.loc)))]
  ), target);
};

let rec dig_base = base =>
  switch (base.pexp_desc) {
  | Pexp_apply(
      {
        pexp_loc: get_loc,
        pexp_desc: Pexp_ident({txt: Ldot(Lident("Array"), "get")}),
      },
      [
        ("", base),
        (
          "",
          {
            pexp_desc: Pexp_constant(Asttypes.Const_string(attr, None)),
            pexp_loc: attr_loc,
          },
        ),
      ],
    ) =>
      let (base, path) = dig_base(base);
      (base, [Location.mkloc(attr, attr_loc), ...path])
  | _ => (base, [])
  };

let id = ref(0);

let get_mapper = {
  ...Ast_mapper.default_mapper,
  expr: (mapper, expr) => {
    switch (expr.pexp_desc) {
      | Pexp_apply({pexp_loc, pexp_desc: Pexp_field({pexp_desc: Pexp_apply(
        {pexp_loc: get_loc, pexp_desc: Pexp_ident({txt: Ldot(Lident("Array"), "get")})},
        [("", base), ("", {pexp_desc: Pexp_constant(Asttypes.Const_string(attr, None)), pexp_loc: attr_loc})]
      )}, {loc: op_loc, txt: Longident.Lident(("replace" | "map") as op)})}, [("", arg)]) => {
        /* awesome["thing"].replace(thing)
        // awesome["thing"].map(one => two)
        // ok, I'm going to need to do an unrolling on any nested things there. That's ok
        */
        let arg = mapper.expr(mapper, arg);
        let base = mapper.expr(mapper, base);

        let (core, path) = dig_base(base);
        let path = List.rev(path);
        let full_path = path @ [Location.mkloc(attr, attr_loc)];
        open Ast_helper;
        let item_ident = Exp.ident(Location.mkloc(Lident("item"), base.pexp_loc));
        /* TODO make the argument also well type-checked */
        let arg_ident = Exp.ident(Location.mkloc(Lident("arg"), arg.pexp_loc));
        let last = op == "map"
        ? Exp.apply(~loc=op_loc, arg_ident, [("", {
          /* one##two##three etc. */
          js_getter(item_ident, full_path)
          /* full_path |> List.fold_left((expr, attr) => Exp.apply(
          //   ~loc=get_loc,
          //   Exp.ident(~loc=get_loc, Location.mkloc(Lident("##"), get_loc)),
          //   [("", expr), ("", Exp.ident(~loc=attr_loc, Location.mkloc(Lident(attr), attr_loc)))]
          // ), base);
          */
        })])
        : arg_ident;

        /**
        Object.assign(
          {},
          item,
          {
            rendering:
              Object.assign(
                {},
                item.rendering,
                {
                  exposure:
                    Object.assign(
                      {},
                      item.rendering.exposure,
                      {awesome: fn(item.rendering.exposure.awesome)}
                    )
                }
              )
          }
        )
         */

        let rec loop = (so_far, args: list(Location.loc(string)), target) => {
          switch args {
            | [] => last
            | [arg, ...rest] =>
              [%expr

              Js.Obj.assign(
                Js.Obj.assign(
                  Js.Obj.empty(),
                  [%e 
                  js_getter(target, so_far)
                  ],
                ),
                [%bs.obj [%e
                Exp.record(
                  [(Location.mkloc(
                    Lident(arg.txt),
                    arg.loc
                  ), loop(so_far @ [arg], rest, target))],
                  None
                )
                ]]
              )
              ]
              }
        };
        id := id^ + 1;
        let suffix = "_js.deep_" ++ string_of_int(id^);

        let argvar = Typ.var("arg" ++ suffix);
        let itemvar = Typ.var("item" ++ suffix);

        let transform_type = Typ.alias(
          List.fold_right((arg, inner) => {
            Typ.constr(
              Location.mknoloc(Ldot(Lident("Js"), "t")),
              [
                Typ.object_(
                  [
                    (
                      arg.Location.txt,
                      [],
                      inner
                    )
                  ],
                  Asttypes.Open
                )
              ]
            )
          },
          full_path,
          argvar
          ),
          "item" ++ suffix
        );

        [%expr {
          let transform = (item: [%t transform_type], arg: [%t
          op == "map" ? [%type: [%t argvar] => [%t argvar]] : argvar
          ]): [%t itemvar] => [%e loop([], full_path, item_ident)];
          transform([%e core], [%e arg])
        }]
      }
      | Pexp_apply({pexp_loc, pexp_desc: Pexp_ident({txt: Lident("#??")})}, [("", arg), ("", name)]) =>
        switch (name.pexp_desc) {
          | Pexp_ident({txt: Lident(_)}) => [%expr switch ([%e mapper.expr(mapper, arg)]) {
            | None => None
            | Some(arg) => arg##[%e name]
          }]
          | _ => fail(pexp_loc, "#? must have a literal string after it")
        }
      | Pexp_apply({pexp_loc, pexp_desc: Pexp_ident({txt: Lident("#?")})}, [("", arg), ("", name)]) =>
        switch (name.pexp_desc) {
          | Pexp_ident({txt: Lident(_)}) => [%expr switch ([%e mapper.expr(mapper, arg)]) {
            | None => None
            | Some(arg) => Some(arg##[%e name])
          }]
          | _ => fail(pexp_loc, "#? must have a literal string after it")
        }
      | _ => Ast_mapper.default_mapper.expr(mapper, expr)
    }
  }
};

let mapper = _argv =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    /* TODO throw error on structure items */
    expr: (mapper, expr) =>
      switch expr.pexp_desc {
      | Pexp_extension(({txt: "js.deep", loc}, PStr([{pstr_desc: Pstr_eval(expr, attributes)}]))) => {
        get_mapper.expr(get_mapper, expr)
      }
      | _ => Ast_mapper.default_mapper.expr(mapper, expr)
      }
  };

let () = Ast_mapper.run_main(mapper);