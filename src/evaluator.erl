-module(evaluator).

-import(do, [fmap/2]).
-import(knife, [parse/1]).
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
            erlang:error(
                io:format("Unbound variable ~p~n", [Name]));
        Result ->
            Result
    end.

evalCondition(Environment, Case, Then, Else) ->
    case evalWithEnvironment(Environment, Case) of
        {ok, {literal, {boolean, true}}} ->
            evalWithEnvironment(Environment, Then);
        {ok, {literal, {boolean, false}}} ->
            evalWithEnvironment(Environment, Else);
        _ ->
            erlang:error(
                io:format("Expression ~p could not be reduced to boolean~n", [Case]))
    end.

evalClosure(ClosedEnvironment,
            Environment,
            Arguments,
            {variadic, TypesAndLabels},
            Body) ->
    Labels = lists:map(fun({_, X}) -> X end, TypesAndLabels),
    if length(Arguments) >= length(Labels) ->
           EvaluatedValues =
               lists:map(fun(X) -> evalWithEnvironment(Environment, X) end, Arguments),
           {NonVariadicArgs, VariadicArgs} = lists:split(length(Labels) - 1, EvaluatedValues),
           {NonVariadicNames, [VariadicName]} = lists:split(length(Labels) - 1, Labels),
           ParamEnv =
               maps:from_list(
                   lists:merge(
                       lists:zip(NonVariadicNames,
                                 lists:map(fun({ok, X}) -> X end, NonVariadicArgs)),
                       [{VariadicName, lists:map(fun({ok, X}) -> X end, VariadicArgs)}])),
           NewEnvironment = maps:merge(ParamEnv, ClosedEnvironment),
           evalWithEnvironment(NewEnvironment, lists:nth(1, Body));
       true ->
           erlang:error(
               io:format("Less arguments than expected for the non variadic part of the "
                         "abstraction that uses ~p and ~p~n.",
                         [Arguments, Labels]))
    end;
evalClosure(ClosedEnvironment,
            Environment,
            Arguments,
            {notVariadic, TypesAndLabels},
            Body) ->
    Labels = lists:map(fun({_, X}) -> X end, TypesAndLabels),
    EvaluatedValues = lists:map(fun(X) -> evalWithEnvironment(Environment, X) end, Arguments),
    ParamEnv =
        maps:from_list(
            lists:zip(Labels, lists:map(fun({ok, X}) -> X end, EvaluatedValues))),
    NewEnvironment = maps:merge(ParamEnv, ClosedEnvironment),
    %% We are getting the head here because we need to adapt the
    %% evaluator to iterate through a list of expressions.
    %% We need to imitate SML do, a.k.a, progn.
    evalWithEnvironment(NewEnvironment, lists:nth(1, Body)).

evalNativeFunction(Environment, Function, Arguments) ->
    EvaluatedValues =
        lists:map(fun(X) ->
                     case evalWithEnvironment(Environment, X) of
                         {ok, Value} -> Value;
                         Value -> Value
                     end
                  end,
                  Arguments),
    Function(EvaluatedValues).

evalApplication(Environment, Abstraction, Arguments) ->
    case evalWithEnvironment(Environment, Abstraction) of
        {ok, {closure, Parameters, Body, ClosedEnvironment}} ->
            evalClosure(ClosedEnvironment, Environment, Arguments, Parameters, Body);
        {ok, {nativeFunc, _, Function}} ->
            evalNativeFunction(Environment, Function, Arguments);
        _ ->
            erlang:error(
                io:format("Could not apply the following value, because it is not a function: "
                          "~p~n",
                          [Abstraction]))
    end.

eval(Expression) ->
    evalWithEnvironment(prelude(), Expression).
