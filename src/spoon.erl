-module(spoon).

-export([eval/0, prelude/0]).

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

evalNativeFunction(Environment, Function, Value) ->
    case evalWithEnvironment(Environment, Value) of
        {ok, Argument} ->
            Function(Argument);
        Error ->
            Error
    end.

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
    Expression = {application, {application, {variable, opDiv}, {literal, 40}}, {literal, 0}},
    case evalWithEnvironment(prelude(), Expression) of
        {ok, Result} ->
            Result;
        {error, Message} ->
            Message
    end.

% [+ 1 2 3 4 5]

% (defun abc (x:string, y: string, z: &rest int))
% (abc "A" "B" 1 2 3 4 5 6 7)

% {node: abstraction, parameter: [x, y, z:[]], parameter_type: [int, int, int[]], body: {node: variable, label: x} }

% [1, 2, ["asd", "xyz"]]

% [int, int, int]
% [1, 2, 3]

% lists:all(fun (Type, {Type2, _}) -> Type == typeOf(Type2) , Arg2)
% [(t_integer, {integer, 1}), (int, 2), (int, 3)]

% integer -> t_integer

% typeOf env expression -> type
