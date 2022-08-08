-module(spoon).

-import(do, [fmap/2]).

-export([eval/0]).

%% (fun x -> x(x)) (fun x -> x(x))
%% {application,
 %% {abstraction, x, {application, {variable, x}, {variable, x}}},
 %% {abstraction, x, {application, {variable, x}, {variable, x}}}}

%% {arithmetic, {operation, add, [{literal, 1}, {literal, 2}, {literal, 3}]}}

evalWithEnvironment(_, {literal, _} = Expression) ->
    {ok, Expression};
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

%% (defun fun (a b &rest c) ())
%% (fun 1 2 3 4 5 6 7)
%% Arguments = [{literal, 1}, {variable, x}, {literal, 2}, {variable, y}, {literal, 4}]
%% Parameters = {isVariadic, [{argument, a}, {argument, b}, {rest, c}]}
%% Check if the arguments and variadics conform with the function annotation
%% Evaluate arguments into a list
%% Zip arguments with labels
%% Add variatic argument to the environment as a list, necessarily
evalClosure(ClosedEnvironment, Environment, Arguments, Parameters, Body) ->
    case Parameters of
        {variadic, Labels} ->
            if length(Arguments) >= length(Labels) ->
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
                   end
            end;
        {notVariadic, Labels} ->
            EvaluatedValues =
                lists:map(fun(X) -> evalWithEnvironment(Environment, X) end, Arguments),
            io:fwrite("A: ~p~n", EvaluatedValues),
            FindAnError =
                lists:search(fun ({error, _}) ->
                                     true;
                                 (_) ->
                                     false
                             end,
                             EvaluatedValues),
            io:fwrite("B: ~p~n", [FindAnError]),
            case FindAnError of
                {value, Error} ->
                    Error;
                false ->
                    ParamEnv =
                        maps:from_list(
                            lists:zip(
                                lists:map(fun({argument, X}) -> X end, Labels),
                                lists:map(fun({ok, X}) -> X end, EvaluatedValues))),
                    io:fwrite("C: ~p~n", [ParamEnv]),
                    NewEnvironment = maps:merge(ParamEnv, ClosedEnvironment),
                    evalWithEnvironment(NewEnvironment, Body)
            end
    end.

prelude() ->
    maps:from_list([add(), subtract(), multiplication(), division()]).

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

genNativeFunc(Operator) ->
    Function =
        fun(Element, Accum) ->
           case {Accum, Element} of
               {{ok, {literal, Acc}}, {ok, {literal, X}}} ->
                   {ok, {literal, Operator(Acc, X)}};
               _ ->
                   {error, "Could not operate on this"}
           end
        end,
    {nativeFunc,
     fun(List) ->
        [Head | Tail] = List,
        io:fwrite("~p~n", [Tail]),
        lists:foldl(Function, Head, Tail)
     end}.

add() ->
    {opAdd, genNativeFunc(fun erlang:'+'/2)}.

subtract() ->
    {opSub, genNativeFunc(fun erlang:'-'/2)}.

multiplication() ->
    {opMul, genNativeFunc(fun erlang:'*'/2)}.

division() ->
    {opDiv,
     genNativeFunc(fun (_, 0) ->
                           erlang:error("Division by zero is not allowed");
                       (A, B) ->
                           A / B
                   end)}.

eval() ->
    %%Expression =
    %%    {application,
    %%     {abstraction, {notVariadic, [{argument, y}]}, {variable, y}},
    %%     [{literal, 1}]},
    %%Expression =
    %%    {application,
    %%    {abstraction, {variadic, [{rest, b}]}, {variable, b}},
    %%     [{literal, 1}, {literal, 2}, {literal, 3}]},
    Expression =
        {application,
         {variable, opDiv},
         [{literal, 1}, {literal, 2}, {literal, 2}, {literal, 2}]},
    evalWithEnvironment(prelude(),
                        Expression).    %%evalWithEnvironment(maps:new(), Expression).
