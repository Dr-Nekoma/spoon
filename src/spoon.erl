-module(spoon).

-import(do, [fmap/2, do/2]).

-export([eval/0, prelude/0, fmap_maybe/0, increment/1, main/1]).

%% (fun x -> x(x)) (fun x -> x(x))
%% {application,
%% {abstraction, x, {application, {variable, x}, {variable, x}}},
%% {abstraction, x, {application, {variable, x}, {variable, x}}}}

%% {arithmetic, {operation, add, [{literal, 1}, {literal, 2}, {literal, 3}]}}

evalWithEnvironment(_, {literal, _} = Expression) ->
    {ok, Expression};
evalWithEnvironment(Environment, {abstraction, Label, Body}) ->
    {ok, {closure, Label, Body, Environment}};
evalWithEnvironment(Environment, {application, Abstraction, Value}) ->
    evalApplication(Environment, Abstraction, Value);
evalWithEnvironment(Environment, {variable, Name}) ->
    case maps:find(Name, Environment) of
        error ->
            {error, string:concat("Unbound variable: ", Name)};
        Result ->
            Result
    end.

evalClosure(ClosedEnvironment, Environment, Value, Label, Body) ->
    case evalWithEnvironment(Environment, Value) of
        {ok, EvaluatedValue} ->
            NewEnvironment = maps:put(Label, EvaluatedValue, ClosedEnvironment),
            evalWithEnvironment(NewEnvironment, Body);
        Error ->
            Error
    end.

evalNativeFunction ( Environment , Function , Value ) -> do ( evalWithEnvironment ( Environment , Value ) , [ fun + / 2 ] ) .

evalApplication(Environment, Abstraction, Value) ->
    case evalWithEnvironment(Environment, Abstraction) of
        {ok, {closure, Label, Body, ClosedEnvironment}} ->
            evalClosure(ClosedEnvironment, Environment, Value, Label, Body);
        {ok, {nativeFunc, Function}} ->
            evalNativeFunction(Environment, Function, Value);
        _ ->
            {error,
             string:concat("Could not apply the following value, because it is not a function: ",
                           Abstraction)}
    end.

genNativeFunc(Operator) ->
    {nativeFunc,
     fun ({literal, X}) ->
             {ok,
              {nativeFunc,
               fun ({literal, Y}) ->
                       {ok, {literal, Operator(X, Y)}};
                   (_) ->
                       {error, ""}
               end}};
         (_) ->
             {error, ""}
     end}.

add() ->
    {opAdd, genNativeFunc(fun(A, B) -> A + B end)}.

subtract() ->
    {opSub, genNativeFunc(fun(A, B) -> A - B end)}.

multiplication() ->
    {opMul, genNativeFunc(fun(A, B) -> A * B end)}.

division() ->
    {opDiv,
     genNativeFunc(fun (_, 0) ->
                           erlang:error("Division by zero is not allowed");
                       (A, B) ->
                           A / B
                   end)}.

prelude() ->
    maps:from_list([add(), subtract(), multiplication(), division()]).

eval() ->
    Expression = {application, {application, {variable, opAdd}, {literal, 40}}, {literal, 2}},
    case evalWithEnvironment(prelude(), Expression) of
        {ok, Result} ->
            Result;
        {error, Message} ->
            Message
    end.

increment(N) ->
    N + 1.

fmap_maybe() ->
    fmap(fun increment/1, {just, 1}).

main(_) ->
    io:fwrite("~p~n", [eval()]).
