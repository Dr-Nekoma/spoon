-module(spoon_test).

-include_lib("eunit/include/eunit.hrl").

eval_test_() ->
    ArithmethicExpr1 =
        {application,
         {variable, '*'},
         [{literal, {integer, 1}},
          {literal, {integer, 2}},
          {literal, {integer, 2}},
          {literal, {integer, 2}}]},
    ArithmethicExpr2 =
        {application,
         {variable, '-'},
         [{literal, {integer, 10}},
          {literal, {integer, 1}},
          {literal, {integer, 1}},
          {literal, {integer, 2}}]},
    ArithmethicExpr3 =
        {application,
         {variable, '+'},
         [{literal, {integer, 2}},
          {literal, {integer, 1}},
          {literal, {integer, 0}},
          {literal, {integer, 2}}]},
    [?_assert(spoon:eval({application,
                          {abstraction, {notVariadic, [{argument, y}]}, {variable, y}},
                          [{literal, {integer, 1}}]})
              =:= {ok, {literal, {integer, 1}}}),
     ?_assert(spoon:eval({application,
                          {abstraction, {variadic, [{rest, b}]}, {variable, b}},
                          [{literal, {integer, 1}},
                           {literal, {integer, 2}},
                           {literal, {integer, 3}}]})
              =:= {ok,
                   [{literal, {integer, 1}}, {literal, {integer, 2}}, {literal, {integer, 3}}]}),
     ?_assert(spoon:eval(ArithmethicExpr1) =:= {ok, {literal, {integer, 8}}}),
     ?_assert(spoon:eval(ArithmethicExpr2) =:= {ok, {literal, {integer, 6}}}),
     ?_assert(spoon:eval(ArithmethicExpr3) =:= {ok, {literal, {integer, 5}}}),
     ?_assert(spoon:eval({condition,
                          {application,
                           {variable, '||'},
                           [{literal, {boolean, true}}, {literal, {boolean, false}}]},
                          ArithmethicExpr1,
                          ArithmethicExpr2})
              =:= spoon:eval(ArithmethicExpr1)),
     ?_assert(spoon:eval({condition,
                          {application,
                           {variable, '&&'},
                           [{literal, {boolean, true}}, {literal, {boolean, false}}]},
                          ArithmethicExpr1,
                          ArithmethicExpr2})
              =:= spoon:eval(ArithmethicExpr2)),
     ?_assert(spoon:eval({application,
                          {variable, '<'},
                          [{literal, {integer, 1}}, {literal, {integer, 1}}]})
              =:= {ok, {literal, {boolean, false}}}),
     ?_assert(spoon:eval({application,
                          {variable, '>'},
                          [{literal, {integer, 2}}, {literal, {integer, 1}}]})
              =:= {ok, {literal, {boolean, true}}}),
     ?_assert(spoon:eval({application,
                          {variable, '<='},
                          [{literal, {integer, 1}}, {literal, {integer, 1}}]})
              =:= {ok, {literal, {boolean, true}}}),
     ?_assert(spoon:eval({application,
                          {variable, '>='},
                          [{literal, {integer, 0}}, {literal, {integer, 1}}]})
              =:= {ok, {literal, {boolean, false}}}),
     ?_assert(spoon:eval({application,
                          {variable, '=:='},
                          [{literal, {integer, 1}}, {literal, {integer, 1}}]})
              =:= {ok, {literal, {boolean, true}}}),
     ?_assert(spoon:eval({application, {variable, 'not'}, [{literal, {boolean, true}}]})
              =:= {ok, {literal, {boolean, false}}})].
