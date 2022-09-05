-module(evaluator).

-import(do, [fmap/2]).
-import(prelude, [prelude/0]).

-export([eval/1]).

evalWithEnvironment(_, {literal, _} = Expression) ->
    {ok, Expression};
evalWithEnvironment(Environment, {condition, Case, Then, Else}) ->
    evalCondition(Environment, Case, Then, Else);
evalWithEnvironment(Environment, {abstraction, Parameters, Body}) ->
    {ok, {closure, Parameters, Body, Environment}};
evalWithEnvironment(Environment, {application, Abstraction, Arguments}) ->
    evalApplication(Environment, Abstraction, Arguments);
evalWithEnvironment(Environment, {variable, Name}) ->
    case maps:find(Name, Environment) of
        error ->
            {error, string:concat("Unbound variable: ", Name)};
        Result ->
            Result
    end.

evalCondition(Environment, Case, Then, Else) ->
    case evalWithEnvironment(Environment, Case) of
        {ok, {literal, {boolean, true}}} -> evalWithEnvironment(Environment, Then);
        {ok, {literal, {boolean, false}}} -> evalWithEnvironment(Environment, Else);
        _ -> erlang:error("Expression could not be reduced to boolean")
    end.

evalClosure(ClosedEnvironment, Environment, Arguments, {variadic, Labels}, Body) ->
    if 
        length(Arguments) >= length(Labels) ->
            EvaluatedValues =
                lists:map(fun(X) -> evalWithEnvironment(Environment, X) end, Arguments),
            FindAnError =
                lists:search(fun ({error, _}) ->
                                    true;
                                (_) ->
                                    false
                            end,
                            EvaluatedValues),
            case FindAnError of
                {value, Error} ->
                    Error;
                false ->
                    {NonVariadicArgs, VariadicArgs} =
                        lists:split(length(Labels) - 1, EvaluatedValues),
                    {NonVariadicNames, [{rest, VariadicName}]} =
                        lists:split(length(Labels) - 1, Labels),
                    ParamEnv =
                        maps:from_list(
                            lists:merge(
                                lists:zip(
                                    lists:map(fun({argument, X}) -> X end, NonVariadicNames),
                                    lists:map(fun({ok, X}) -> X end, NonVariadicArgs)),
                                [{VariadicName,
                                    lists:map(fun({ok, X}) -> X end, VariadicArgs)}])),

                    NewEnvironment = maps:merge(ParamEnv, ClosedEnvironment),
                    evalWithEnvironment(NewEnvironment, Body)
            end;
        true -> erlang:error("Less arguments than expected for the non variadic part of the abstraction.")
    end;
            
evalClosure(ClosedEnvironment, Environment, Arguments, {notVariadic, Labels}, Body) ->
    EvaluatedValues =
        lists:map(fun(X) -> evalWithEnvironment(Environment, X) end, Arguments),
    FindAnError =
        lists:search(fun ({error, _}) ->
                                true;
                            (_) ->
                                false
                        end,
                        EvaluatedValues),
    case FindAnError of
        {value, Error} ->
            Error;
        false ->
            ParamEnv =
                maps:from_list(
                    lists:zip(
                        lists:map(fun({argument, X}) -> X end, Labels),
                        lists:map(fun({ok, X}) -> X end, EvaluatedValues))),
            NewEnvironment = maps:merge(ParamEnv, ClosedEnvironment),
            evalWithEnvironment(NewEnvironment, Body)
    end.

evalNativeFunction(Environment, Function, Arguments) ->
    EvaluatedValues = lists:map(fun(X) -> evalWithEnvironment(Environment, X) end, Arguments),
    FindAnError =
        lists:search(fun ({error, _}) ->
                             true;
                         (_) ->
                             false
                     end,
                     EvaluatedValues),

    case FindAnError of
        {value, Error} ->
            Error;
        false ->
            Function(EvaluatedValues)
    end.

evalApplication(Environment, Abstraction, Arguments) ->
    case evalWithEnvironment(Environment, Abstraction) of
        {ok, {closure, Parameters, Body, ClosedEnvironment}} ->
            evalClosure(ClosedEnvironment, Environment, Arguments, Parameters, Body);
        {ok, {nativeFunc, Function}} ->
            evalNativeFunction(Environment, Function, Arguments);
        _ ->
            {error,
             string:concat("Could not apply the following value, because it is not a function: ",
                           Abstraction)}
    end.


eval(Expression) -> evalWithEnvironment(prelude(), Expression).
