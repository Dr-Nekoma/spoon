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

genNativeArithmeticFunc(ArithmeticOperator) ->
    Function =
        fun(Element, Accum) ->
           case {Accum, Element} of
               {{ok, {literal, {integer, Acc}}}, {ok, {literal, {integer, X}}}} ->
                   {ok, {literal, {integer, ArithmeticOperator(Acc, X)}}};
               _ ->
                   {error, "Could not operate on this since it's not a number"}
           end
        end,
    {nativeFunc,
     fun(List) ->
        [Head | Tail] = List,
        lists:foldl(Function, Head, Tail)
     end}.

genNativeComparisonFunc(Operator) ->
    {nativeFunc,
        fun([{ok, {literal, {integer, Left}}}, {ok, {literal, {integer, Right}}}]) ->
            {ok, {literal, {boolean, Operator(Left, Right)}}};
           (_) -> erlang:error("Could not eval valid comparison expression with arity 2")
	    end}.

genNativeLogicalFunc(BoolOperator, MEmpty) ->
    Function =
        fun(Element, Accum) ->
            case {Accum, Element} of
                {{ok, {literal, {boolean, Acc}}}, {ok, {literal, {boolean, X}}}} ->
                    {ok, {literal, {boolean, BoolOperator(Acc, X)}}};
                _ ->
                    {error, "Could not operate on this since it's not a boolean"}
            end
        end,
    {nativeFunc,
        fun(List) ->
            lists:foldl(Function, {ok, {literal, {boolean, MEmpty}}}, List)
        end}.

boolLessThan() ->
    {'<', genNativeComparisonFunc(fun erlang:'<'/2)}.

boolGreaterThan() ->
    {'>', genNativeComparisonFunc(fun erlang:'>'/2)}.

boolEqual() ->
    {'=', genNativeComparisonFunc(fun erlang:'='/2)}.

boolGreaterEqualThan() ->
    {'>=', genNativeComparisonFunc(fun erlang:'>='/2)}.

boolLessEqualThan() ->
    {'<=', genNativeComparisonFunc(fun erlang:'=<'/2)}.

boolAnd() ->
    {'&&', genNativeLogicalFunc(fun erlang:'and'/2, true)}.
    
boolOr() ->
    {'||', genNativeLogicalFunc(fun erlang:'or'/2, false)}.

arithAdd() ->
    {'+', genNativeArithmeticFunc(fun erlang:'+'/2)}.

arithSubtract() ->
    {'-', genNativeArithmeticFunc(fun erlang:'-'/2)}.

arithMultiplication() ->
    {'*', genNativeArithmeticFunc(fun erlang:'*'/2)}.

arithDivision() ->
    {'/',
    genNativeArithmeticFunc(fun (_, 0) ->
                           erlang:error("Division by zero is not allowed");
                       (A, B) ->
                           A / B
                   end)}.                


prelude() ->
    maps:from_list([arithAdd(), 
                    arithSubtract(), 
                    arithMultiplication(), 
                    arithDivision(), 
                    boolAnd(), 
                    boolOr(), 
                    boolGreaterEqualThan(), 
                    boolLessEqualThan(), 
                    boolEqual(),
                    boolLessThan(),
                    boolGreaterThan()]).

eval() ->
    %%Expression =
    %%    {application,
    %%     {abstraction, {notVariadic, [{argument, y}]}, {variable, y}},
    %%     [{literal, 1}]},
    %%Expression =
    %%    {application,
    %%    {abstraction, {variadic, [{rest, b}]}, {variable, b}},
    %%     [{literal, 1}, {literal, 2}, {literal, 3}]},
    ArithmeticExpr1 =
        {application,
         {variable, '/'},
         [{literal, {integer, 1}}, {literal, {integer, 2}}, {literal, {integer, 2}}, {literal, {integer, 2}}]},
    ArithmeticExpr2 =
        {application,
         {variable, '*'},
         [{literal, {integer, 1}}, {literal, {integer, 2}}, {literal, {integer, 2}}, {literal, {integer, 2}}]},
    %% Expression =
    %%     {condition, {application, {variable, opOr}, [{literal, {boolean, true}}, {literal, {boolean, false}}]}, ArithmeticExpr1, ArithmeticExpr2},
    Expression =
        {application, {variable, '<'}, [{literal, {integer, 1}}, {literal, {integer, 1}}]},

    evalWithEnvironment(prelude(), Expression).    %%evalWithEnvironment(maps:new(), Expression).
