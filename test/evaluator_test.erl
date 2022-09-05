-module(evaluator_test).

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
    [?_assertEqual(evaluator:eval({application,
                          {abstraction, {notVariadic, [{argument, y}]}, {variable, y}},
                          [{literal, {integer, 1}}]})
              , {ok, {literal, {integer, 1}}}),
     ?_assertEqual(evaluator:eval({application,
                          {abstraction, {variadic, [{rest, b}]}, {variable, b}},
                          [{literal, {integer, 1}},
                           {literal, {integer, 2}},
                           {literal, {integer, 3}}]})
              , {ok,
                   [{literal, {integer, 1}}, {literal, {integer, 2}}, {literal, {integer, 3}}]}),
     ?_assertEqual(evaluator:eval({application,
                          {abstraction, {variadic, [{argument, a}, {argument, b}, {rest, c}]}, {variable, b}},
                          [{literal, {integer, 1}},
                           {literal, {integer, 2}},
                           {literal, {integer, 3}},
                           {literal, {integer, 4}}]})
              , {ok, {literal, {integer, 2}}}),
     ?_assertEqual(evaluator:eval(ArithmethicExpr1) , {ok, {literal, {integer, 8}}}),
     ?_assertEqual(evaluator:eval(ArithmethicExpr2) , {ok, {literal, {integer, 6}}}),
     ?_assertEqual(evaluator:eval(ArithmethicExpr3) , {ok, {literal, {integer, 5}}}),
     ?_assertEqual(evaluator:eval({condition,
                          {application,
                           {variable, '||'},
                           [{literal, {boolean, true}}, {literal, {boolean, false}}]},
                          ArithmethicExpr1,
                          ArithmethicExpr2})
              , evaluator:eval(ArithmethicExpr1)),
     ?_assertEqual(evaluator:eval({condition,
                          {application,
                           {variable, '&&'},
                           [{literal, {boolean, true}}, {literal, {boolean, false}}]},
                          ArithmethicExpr1,
                          ArithmethicExpr2})
              , evaluator:eval(ArithmethicExpr2)),
     ?_assertEqual(evaluator:eval({application,
                          {variable, '<'},
                          [{literal, {integer, 1}}, {literal, {integer, 1}}]})
              , {ok, {literal, {boolean, false}}}),
     ?_assertEqual(evaluator:eval({application,
                          {variable, '>'},
                          [{literal, {integer, 2}}, {literal, {integer, 1}}]})
              , {ok, {literal, {boolean, true}}}),
     ?_assertEqual(evaluator:eval({application,
                          {variable, '<='},
                          [{literal, {integer, 1}}, {literal, {integer, 1}}]})
              , {ok, {literal, {boolean, true}}}),
     ?_assertEqual(evaluator:eval({application,
                          {variable, '>='},
                          [{literal, {integer, 0}}, {literal, {integer, 1}}]})
              , {ok, {literal, {boolean, false}}}),
     ?_assertEqual(evaluator:eval({application,
                          {variable, '=:='},
                          [{literal, {integer, 1}}, {literal, {integer, 1}}]})
              , {ok, {literal, {boolean, true}}}),
     ?_assertEqual(evaluator:eval({application, {variable, 'not'}, [{literal, {boolean, true}}]})
              , {ok, {literal, {boolean, false}}})].
