-module(typer_test).

-include_lib("eunit/include/eunit.hrl").

typeCheck_test_() -> [
    ?_assertEqual(typer:typeCheck({literal, {integer, 1}})
              , integer),
    ?_assertEqual(typer:typeCheck({literal, {boolean, true}})
              , boolean),
    ?_assertEqual(typer:typeCheck({condition, {literal, {boolean, 1}}, {literal, {integer, 1}}, {literal, {integer, 1}}})
              , integer),
    ?_assertError(_, typer:typeCheck({condition, {literal, {integer, 1}}, {literal, {integer, 1}}, {literal, {integer, 1}}})),
    ?_assertError(_, typer:typeCheck({condition, {literal, {boolean, true}}, {literal, {integer, 1}}, {literal, {string, "Hello"}}})),
    ?_assertEqual(typer:typeCheck({abstraction, {notVariadic, [{integer, a}]}, {literal, {integer, 1}}})
              , {function, notVariadic, [integer], integer}),
    ?_assertNotEqual(typer:typeCheck({abstraction, {notVariadic, [{integer, a}]}, {literal, {integer, 1}}})
              , {function, variadic, [integer], integer}),
    ?_assertNotEqual(typer:typeCheck({abstraction, {notVariadic, [{integer, a}]}, {literal, {integer, 1}}})
              , {function, notVariadic, [integer, integer], integer}),
    ?_assertNotEqual(typer:typeCheck({abstraction, {notVariadic, [{integer, a}]}, {literal, {integer, 1}}})
              , {function, notVariadic, [integer], boolean}),
    ?_assertNotEqual(typer:typeCheck({abstraction, {notVariadic, [{integer, a}]}, {literal, {integer, 1}}})
              , {function, notVariadic, [], integer}),
    ?_assertEqual(typer:typeCheck({abstraction, {variadic, [{integer, a}, {integer, rest}]}, {variable, a}})
              , {function, variadic, [integer, integer], integer}),
    ?_assertError(_, typer:typeCheck(
                {application, 
                    {abstraction, 
                        {variadic, [{integer, a}, {integer, rest}]}, 
                        {variable, a}}, 
                    [{literal, {integer, 1}}, {literal, {string, 2}}]})),
    ?_assertEqual(typer:typeCheck(
                        {application, 
                            {abstraction, 
                                {variadic, [{integer, a}, {integer, rest}]}, 
                                {variable, a}}, 
                            [{literal, {integer, 1}}, {literal, {integer, 2}}]}),
                    integer)].