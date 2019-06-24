[@ocaml.ppx.context {cookies: []}];
let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));

open Parsetree;
open Longident;

let js_getter = (target, path) =>
  Ast_helper.(
    path
    |> List.fold_left(
         (expr, attr: Location.loc(string)) =>
           Exp.apply(
             ~loc=attr.loc,
             Exp.ident(
               ~loc=attr.Location.loc,
               Location.mkloc(Lident("##"), attr.Location.loc),
             ),
             [
               ("", expr),
               (
                 "",
                 Exp.ident(
                   ~loc=attr.Location.loc,
                   Location.mkloc(Lident(attr.txt), attr.loc),
                 ),
               ),
             ],
           ),
         target,
       )
  );

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
    (base, [Location.mkloc(attr, attr_loc), ...path]);
  | _ => (base, [])
  };

let id = ref(0);

let get_mapper = {
  ...Ast_mapper.default_mapper,
  expr: (mapper, expr) =>
    switch (expr.pexp_desc) {
    | Pexp_apply(
        {
          pexp_loc,
          pexp_desc:
            Pexp_field(
              {
                pexp_desc:
                  Pexp_apply(
                    {
                      pexp_loc: get_loc,
                      pexp_desc:
                        Pexp_ident({txt: Ldot(Lident("Array"), "get")}),
                    },
                    [
                      ("", base),
                      (
                        "",
                        {
                          pexp_desc:
                            Pexp_constant(Asttypes.Const_string(attr, None)),
                          pexp_loc: attr_loc,
                        },
                      ),
                    ],
                  ),
              },
              {
                loc: op_loc,
                txt: Longident.Lident(("replace" | "map") as op),
              },
            ),
        },
        [("", arg)],
      ) =>
      let arg = mapper.expr(mapper, arg);
      let base = mapper.expr(mapper, base);

      let (core, path) = dig_base(base);
      let path = List.rev(path);
      let full_path = path @ [Location.mkloc(attr, attr_loc)];
      open Ast_helper;
      let item_ident =
        Exp.ident(Location.mkloc(Lident("item"), base.pexp_loc));

      let arg_ident =
        Exp.ident(Location.mkloc(Lident("arg"), arg.pexp_loc));
      let last =
        op == "map" ?
          Exp.apply(
            ~loc=op_loc,
            arg_ident,
            [("", js_getter(item_ident, full_path))],
          ) :
          arg_ident;

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

      let rec loop = (so_far, args: list(Location.loc(string)), target) =>
        switch (args) {
        | [] => last
        | [arg, ...rest] => {
            Parsetree.pexp_desc:
              [@implicit_arity]
              Parsetree.Pexp_apply(
                {
                  Parsetree.pexp_desc:
                    Parsetree.Pexp_ident({
                      Asttypes.txt:
                        [@implicit_arity]
                        Longident.Ldot(
                          [@implicit_arity]
                          Longident.Ldot(Longident.Lident("Js"), "Obj"),
                          "assign",
                        ),
                      Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc),
                    }),
                  Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                  Parsetree.pexp_attributes: [],
                },
                [
                  (
                    "",
                    {
                      Parsetree.pexp_desc:
                        [@implicit_arity]
                        Parsetree.Pexp_apply(
                          {
                            Parsetree.pexp_desc:
                              Parsetree.Pexp_ident({
                                Asttypes.txt:
                                  [@implicit_arity]
                                  Longident.Ldot(
                                    [@implicit_arity]
                                    Longident.Ldot(
                                      Longident.Lident("Js"),
                                      "Obj",
                                    ),
                                    "assign",
                                  ),
                                Asttypes.loc:
                                  Pervasives.(^)(Ast_helper.default_loc),
                              }),
                            Parsetree.pexp_loc:
                              Pervasives.(^)(Ast_helper.default_loc),
                            Parsetree.pexp_attributes: [],
                          },
                          [
                            (
                              "",
                              {
                                Parsetree.pexp_desc:
                                  [@implicit_arity]
                                  Parsetree.Pexp_apply(
                                    {
                                      Parsetree.pexp_desc:
                                        Parsetree.Pexp_ident({
                                          Asttypes.txt:
                                            [@implicit_arity]
                                            Longident.Ldot(
                                              [@implicit_arity]
                                              Longident.Ldot(
                                                Longident.Lident("Js"),
                                                "Obj",
                                              ),
                                              "empty",
                                            ),
                                          Asttypes.loc:
                                            Pervasives.(^)(
                                              Ast_helper.default_loc,
                                            ),
                                        }),
                                      Parsetree.pexp_loc:
                                        Pervasives.(^)(
                                          Ast_helper.default_loc,
                                        ),
                                      Parsetree.pexp_attributes: [],
                                    },
                                    [
                                      (
                                        "",
                                        {
                                          Parsetree.pexp_desc:
                                            [@implicit_arity]
                                            Parsetree.Pexp_construct(
                                              {
                                                Asttypes.txt:
                                                  Longident.Lident("()"),
                                                Asttypes.loc:
                                                  Pervasives.(^)(
                                                    Ast_helper.default_loc,
                                                  ),
                                              },
                                              None,
                                            ),
                                          Parsetree.pexp_loc:
                                            Pervasives.(^)(
                                              Ast_helper.default_loc,
                                            ),
                                          Parsetree.pexp_attributes: [],
                                        },
                                      ),
                                    ],
                                  ),
                                Parsetree.pexp_loc:
                                  Pervasives.(^)(Ast_helper.default_loc),
                                Parsetree.pexp_attributes: [],
                              },
                            ),
                            ("", js_getter(target, so_far)),
                          ],
                        ),
                      Parsetree.pexp_loc:
                        Pervasives.(^)(Ast_helper.default_loc),
                      Parsetree.pexp_attributes: [],
                    },
                  ),
                  (
                    "",
                    {
                      Parsetree.pexp_desc:
                        [@implicit_arity]
                        Parsetree.Pexp_extension(
                          {
                            Asttypes.txt: "bs.obj",
                            Asttypes.loc:
                              Pervasives.(^)(Ast_helper.default_loc),
                          },
                          Parsetree.PStr([
                            {
                              Parsetree.pstr_desc:
                                [@implicit_arity]
                                Parsetree.Pstr_eval(
                                  Exp.record(
                                    [
                                      (
                                        Location.mkloc(
                                          Lident(arg.txt),
                                          arg.loc,
                                        ),
                                        loop(so_far @ [arg], rest, target),
                                      ),
                                    ],
                                    None,
                                  ),
                                  [],
                                ),
                              Parsetree.pstr_loc:
                                Pervasives.(^)(Ast_helper.default_loc),
                            },
                          ]),
                        ),
                      Parsetree.pexp_loc:
                        Pervasives.(^)(Ast_helper.default_loc),
                      Parsetree.pexp_attributes: [],
                    },
                  ),
                ],
              ),
            Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
            Parsetree.pexp_attributes: [],
          }
        };
      id := id^ + 1;
      let suffix = "_js.deep_" ++ string_of_int(id^);

      let argvar = Typ.var("arg" ++ suffix);
      let itemvar = Typ.var("item" ++ suffix);

      let transform_type =
        Typ.alias(
          List.fold_right(
            (arg, inner) =>
              Typ.constr(
                Location.mknoloc(Ldot(Lident("Js"), "t")),
                [
                  Typ.object_(
                    [(arg.Location.txt, [], inner)],
                    Asttypes.Open,
                  ),
                ],
              ),
            full_path,
            argvar,
          ),
          "item" ++ suffix,
        );
      {
        Parsetree.pexp_desc:
          [@implicit_arity]
          Parsetree.Pexp_let(
            Asttypes.Nonrecursive,
            [
              {
                Parsetree.pvb_pat: {
                  Parsetree.ppat_desc:
                    Parsetree.Ppat_var({
                      Asttypes.txt: "transform",
                      Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc),
                    }),
                  Parsetree.ppat_loc: Pervasives.(^)(Ast_helper.default_loc),
                  Parsetree.ppat_attributes: [],
                },
                Parsetree.pvb_expr: {
                  Parsetree.pexp_desc:
                    [@implicit_arity]
                    Parsetree.Pexp_fun(
                      "",
                      None,
                      {
                        Parsetree.ppat_desc:
                          [@implicit_arity]
                          Parsetree.Ppat_constraint(
                            {
                              Parsetree.ppat_desc:
                                Parsetree.Ppat_var({
                                  Asttypes.txt: "item",
                                  Asttypes.loc:
                                    Pervasives.(^)(Ast_helper.default_loc),
                                }),
                              Parsetree.ppat_loc:
                                Pervasives.(^)(Ast_helper.default_loc),
                              Parsetree.ppat_attributes: [],
                            },
                            transform_type,
                          ),
                        Parsetree.ppat_loc:
                          Pervasives.(^)(Ast_helper.default_loc),
                        Parsetree.ppat_attributes: [],
                      },
                      {
                        Parsetree.pexp_desc:
                          [@implicit_arity]
                          Parsetree.Pexp_fun(
                            "",
                            None,
                            {
                              Parsetree.ppat_desc:
                                [@implicit_arity]
                                Parsetree.Ppat_constraint(
                                  {
                                    Parsetree.ppat_desc:
                                      Parsetree.Ppat_var({
                                        Asttypes.txt: "arg",
                                        Asttypes.loc:
                                          Pervasives.(^)(
                                            Ast_helper.default_loc,
                                          ),
                                      }),
                                    Parsetree.ppat_loc:
                                      Pervasives.(^)(Ast_helper.default_loc),
                                    Parsetree.ppat_attributes: [],
                                  },
                                  op == "map" ?
                                    {
                                      Parsetree.ptyp_desc:
                                        [@implicit_arity]
                                        Parsetree.Ptyp_arrow(
                                          "",
                                          argvar,
                                          argvar,
                                        ),
                                      Parsetree.ptyp_loc:
                                        Pervasives.(^)(
                                          Ast_helper.default_loc,
                                        ),
                                      Parsetree.ptyp_attributes: [],
                                    } :
                                    argvar,
                                ),
                              Parsetree.ppat_loc:
                                Pervasives.(^)(Ast_helper.default_loc),
                              Parsetree.ppat_attributes: [],
                            },
                            {
                              Parsetree.pexp_desc:
                                [@implicit_arity]
                                Parsetree.Pexp_constraint(
                                  loop([], full_path, item_ident),
                                  itemvar,
                                ),
                              Parsetree.pexp_loc:
                                Pervasives.(^)(Ast_helper.default_loc),
                              Parsetree.pexp_attributes: [],
                            },
                          ),
                        Parsetree.pexp_loc:
                          Pervasives.(^)(Ast_helper.default_loc),
                        Parsetree.pexp_attributes: [],
                      },
                    ),
                  Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                  Parsetree.pexp_attributes: [],
                },
                Parsetree.pvb_attributes: [],
                Parsetree.pvb_loc: Pervasives.(^)(Ast_helper.default_loc),
              },
            ],
            {
              Parsetree.pexp_desc:
                [@implicit_arity]
                Parsetree.Pexp_apply(
                  {
                    Parsetree.pexp_desc:
                      Parsetree.Pexp_ident({
                        Asttypes.txt: Longident.Lident("transform"),
                        Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc),
                      }),
                    Parsetree.pexp_loc:
                      Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.pexp_attributes: [],
                  },
                  [("", core), ("", arg)],
                ),
              Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
              Parsetree.pexp_attributes: [],
            },
          ),
        Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
        Parsetree.pexp_attributes: [],
      };
    | Pexp_apply(
        {pexp_loc, pexp_desc: Pexp_ident({txt: Lident("#??")})},
        [("", arg), ("", name)],
      ) =>
      switch (name.pexp_desc) {

      | Pexp_ident({txt: Lident(_)}) => {
          Parsetree.pexp_desc:
            [@implicit_arity]
            Parsetree.Pexp_match(
              mapper.expr(mapper, arg),
              [
                {
                  Parsetree.pc_lhs: {
                    Parsetree.ppat_desc:
                      [@implicit_arity]
                      Parsetree.Ppat_construct(
                        {
                          Asttypes.txt: Longident.Lident("None"),
                          Asttypes.loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                        },
                        None,
                      ),
                    Parsetree.ppat_loc:
                      Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.ppat_attributes: [],
                  },
                  Parsetree.pc_guard: None,
                  Parsetree.pc_rhs: {
                    Parsetree.pexp_desc:
                      [@implicit_arity]
                      Parsetree.Pexp_construct(
                        {
                          Asttypes.txt: Longident.Lident("None"),
                          Asttypes.loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                        },
                        None,
                      ),
                    Parsetree.pexp_loc:
                      Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.pexp_attributes: [],
                  },
                },
                {
                  Parsetree.pc_lhs: {
                    Parsetree.ppat_desc:
                      [@implicit_arity]
                      Parsetree.Ppat_construct(
                        {
                          Asttypes.txt: Longident.Lident("Some"),
                          Asttypes.loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                        },
                        Some({
                          Parsetree.ppat_desc:
                            Parsetree.Ppat_tuple([
                              {
                                Parsetree.ppat_desc:
                                  Parsetree.Ppat_var({
                                    Asttypes.txt: "arg",
                                    Asttypes.loc:
                                      Pervasives.(^)(Ast_helper.default_loc),
                                  }),
                                Parsetree.ppat_loc:
                                  Pervasives.(^)(Ast_helper.default_loc),
                                Parsetree.ppat_attributes: [],
                              },
                            ]),
                          Parsetree.ppat_loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                          Parsetree.ppat_attributes: [],
                        }),
                      ),
                    Parsetree.ppat_loc:
                      Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.ppat_attributes: [
                      (
                        {
                          Asttypes.txt: "explicit_arity",
                          Asttypes.loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                        },
                        Parsetree.PStr([]),
                      ),
                    ],
                  },
                  Parsetree.pc_guard: None,
                  Parsetree.pc_rhs: {
                    Parsetree.pexp_desc:
                      [@implicit_arity]
                      Parsetree.Pexp_apply(
                        {
                          Parsetree.pexp_desc:
                            Parsetree.Pexp_ident({
                              Asttypes.txt: Longident.Lident("##"),
                              Asttypes.loc:
                                Pervasives.(^)(Ast_helper.default_loc),
                            }),
                          Parsetree.pexp_loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                          Parsetree.pexp_attributes: [],
                        },
                        [
                          (
                            "",
                            {
                              Parsetree.pexp_desc:
                                Parsetree.Pexp_ident({
                                  Asttypes.txt: Longident.Lident("arg"),
                                  Asttypes.loc:
                                    Pervasives.(^)(Ast_helper.default_loc),
                                }),
                              Parsetree.pexp_loc:
                                Pervasives.(^)(Ast_helper.default_loc),
                              Parsetree.pexp_attributes: [],
                            },
                          ),
                          ("", name),
                        ],
                      ),
                    Parsetree.pexp_loc:
                      Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.pexp_attributes: [],
                  },
                },
              ],
            ),
          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
          Parsetree.pexp_attributes: [],
        }

      | _ => fail(pexp_loc, "#? must have a literal string after it")
      }
    | Pexp_apply(
        {pexp_loc, pexp_desc: Pexp_ident({txt: Lident("#?")})},
        [("", arg), ("", name)],
      ) =>
      switch (name.pexp_desc) {

      | Pexp_ident({txt: Lident(_)}) => {
          Parsetree.pexp_desc:
            [@implicit_arity]
            Parsetree.Pexp_match(
              mapper.expr(mapper, arg),
              [
                {
                  Parsetree.pc_lhs: {
                    Parsetree.ppat_desc:
                      [@implicit_arity]
                      Parsetree.Ppat_construct(
                        {
                          Asttypes.txt: Longident.Lident("None"),
                          Asttypes.loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                        },
                        None,
                      ),
                    Parsetree.ppat_loc:
                      Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.ppat_attributes: [],
                  },
                  Parsetree.pc_guard: None,
                  Parsetree.pc_rhs: {
                    Parsetree.pexp_desc:
                      [@implicit_arity]
                      Parsetree.Pexp_construct(
                        {
                          Asttypes.txt: Longident.Lident("None"),
                          Asttypes.loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                        },
                        None,
                      ),
                    Parsetree.pexp_loc:
                      Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.pexp_attributes: [],
                  },
                },
                {
                  Parsetree.pc_lhs: {
                    Parsetree.ppat_desc:
                      [@implicit_arity]
                      Parsetree.Ppat_construct(
                        {
                          Asttypes.txt: Longident.Lident("Some"),
                          Asttypes.loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                        },
                        Some({
                          Parsetree.ppat_desc:
                            Parsetree.Ppat_tuple([
                              {
                                Parsetree.ppat_desc:
                                  Parsetree.Ppat_var({
                                    Asttypes.txt: "arg",
                                    Asttypes.loc:
                                      Pervasives.(^)(Ast_helper.default_loc),
                                  }),
                                Parsetree.ppat_loc:
                                  Pervasives.(^)(Ast_helper.default_loc),
                                Parsetree.ppat_attributes: [],
                              },
                            ]),
                          Parsetree.ppat_loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                          Parsetree.ppat_attributes: [],
                        }),
                      ),
                    Parsetree.ppat_loc:
                      Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.ppat_attributes: [
                      (
                        {
                          Asttypes.txt: "explicit_arity",
                          Asttypes.loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                        },
                        Parsetree.PStr([]),
                      ),
                    ],
                  },
                  Parsetree.pc_guard: None,
                  Parsetree.pc_rhs: {
                    Parsetree.pexp_desc:
                      [@implicit_arity]
                      Parsetree.Pexp_construct(
                        {
                          Asttypes.txt: Longident.Lident("Some"),
                          Asttypes.loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                        },
                        Some({
                          Parsetree.pexp_desc:
                            Parsetree.Pexp_tuple([
                              {
                                Parsetree.pexp_desc:
                                  [@implicit_arity]
                                  Parsetree.Pexp_apply(
                                    {
                                      Parsetree.pexp_desc:
                                        Parsetree.Pexp_ident({
                                          Asttypes.txt:
                                            Longident.Lident("##"),
                                          Asttypes.loc:
                                            Pervasives.(^)(
                                              Ast_helper.default_loc,
                                            ),
                                        }),
                                      Parsetree.pexp_loc:
                                        Pervasives.(^)(
                                          Ast_helper.default_loc,
                                        ),
                                      Parsetree.pexp_attributes: [],
                                    },
                                    [
                                      (
                                        "",
                                        {
                                          Parsetree.pexp_desc:
                                            Parsetree.Pexp_ident({
                                              Asttypes.txt:
                                                Longident.Lident("arg"),
                                              Asttypes.loc:
                                                Pervasives.(^)(
                                                  Ast_helper.default_loc,
                                                ),
                                            }),
                                          Parsetree.pexp_loc:
                                            Pervasives.(^)(
                                              Ast_helper.default_loc,
                                            ),
                                          Parsetree.pexp_attributes: [],
                                        },
                                      ),
                                      ("", name),
                                    ],
                                  ),
                                Parsetree.pexp_loc:
                                  Pervasives.(^)(Ast_helper.default_loc),
                                Parsetree.pexp_attributes: [],
                              },
                            ]),
                          Parsetree.pexp_loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                          Parsetree.pexp_attributes: [],
                        }),
                      ),
                    Parsetree.pexp_loc:
                      Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.pexp_attributes: [
                      (
                        {
                          Asttypes.txt: "explicit_arity",
                          Asttypes.loc:
                            Pervasives.(^)(Ast_helper.default_loc),
                        },
                        Parsetree.PStr([]),
                      ),
                    ],
                  },
                },
              ],
            ),
          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
          Parsetree.pexp_attributes: [],
        }

      | _ => fail(pexp_loc, "#? must have a literal string after it")
      }
    | _ => Ast_mapper.default_mapper.expr(mapper, expr)
    },
};

let mapper = _argv =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    expr: (mapper, expr) =>
      switch (expr.pexp_desc) {
      | Pexp_extension((
          {txt: "js.deep", loc},
          PStr([{pstr_desc: Pstr_eval(expr, attributes)}]),
        )) =>
        get_mapper.expr(get_mapper, expr)
      | _ => Ast_mapper.default_mapper.expr(mapper, expr)
      },
  };

let () = Ast_mapper.run_main(mapper);
