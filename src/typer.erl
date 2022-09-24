-module(typer).

-export([typeCheck/1]).

-import(prelude, [prelude/0]).

%% {literal, {integer, 1}}

typeCheckWithEnvironment(_, {literal, {Type, _}}) ->
    Type;
typeCheckWithEnvironment(Environment, {condition, Case, Then, Else}) ->
    typeCheckCondition(Environment, Case, Then, Else);
typeCheckWithEnvironment(Environment, {abstraction, Parameters, Body}) ->
    typeCheckAbstraction(Environment, Parameters, Body);
typeCheckWithEnvironment(Environment, {application, Abstraction, Arguments}) ->
    typeCheckApplication(Environment, Abstraction, Arguments);
typeCheckWithEnvironment(Environment, Variable) ->
    typeCheckVariable(Environment, Variable).

typeCheckCondition(Environment, Case, Then, Else) ->
    TypeCase = typeCheckWithEnvironment(Environment, Case),
    TypeThen = typeCheckWithEnvironment(Environment, Then),
    TypeElse = typeCheckWithEnvironment(Environment, Else),
    case {TypeThen =:= TypeElse, TypeCase =:= boolean} of
        {_, false} ->
            erlang:error(io:format("Condition must be a boolean and found ~p~n", [TypeCase]));
        {false, _} ->
            erlang:error(io:format("Else branch of type ~p does not conform with expected type ~p of branch Then~n.",
                         [TypeElse, TypeThen]));
        _ ->
            TypeThen
    end.

uniq([]) ->
    [];
uniq(List) when is_list(List) ->
    uniq(List, []).

uniq([], Acc) ->
    lists:reverse(Acc);
uniq([H | Tail], Acc) ->
    case lists:member(H, Acc) of
        true ->
            uniq(Tail, Acc);
        false ->
            uniq(Tail, [H | Acc])
    end.
								  
typeCheckAbstraction(Environment, {VariadicInfo, Labels}, Body) ->
    % when the parameter is variadic, it needs to have the list type on the function body
    UniqueList = uniq(lists:map(fun({_, X}) -> X end, Labels)),
    Types = lists:map(fun({X, _}) -> X end, Labels),
    if length(UniqueList) =:= length(Labels) ->
           Function = fun({Type, Name}, Accum) -> maps:put(Name, Type, Accum) end,
           NewEnvironment = lists:foldl(Function, Environment, Labels),
           ReturnType = typeCheckWithEnvironment(NewEnvironment, lists:last(Body)),
           {function, VariadicInfo, Types, ReturnType};
       true ->
           erlang:error(io:format("There are duplicated names in abstraction ~p~n", [Labels]))
    end.

%% typeCheckVariable(Environment, WTF) ->
%%     io:fwrite("~p ~p~n", [Environment, WTF]);

typeCheckVariable(Environment, {variable, Name}) ->
    case maps:find(Name, Environment) of
	{ok, {nativeFunc, Type, _}} ->
	    Type;
        {ok, Type} ->
            Type;
        _ ->
            erlang:error(io:format("Didn't find typed variable ~p~n", [Name]))
    end.

typeCheckApplication(Environment, Abstraction, Arguments) ->
    ArgumentsTypes =
        lists:map(fun(X) -> typeCheckWithEnvironment(Environment, X) end, Arguments),
    case typeCheckWithEnvironment(Environment, Abstraction) of
        {function, notVariadic, Types, ReturnType} ->
            if Types =:= ArgumentsTypes ->
                   ReturnType;
               true ->
                   erlang:error(io:format("Arguments ~p types do not match the parameters types of the abstraction ~p~n",
                                [Arguments, Abstraction]))
            end;
        {function, variadic, Types, ReturnType} ->
            NonVarAmount = length(Types) - 1,
            {NonVarTypeArgs, VarTypeArgs} = lists:split(NonVarAmount, ArgumentsTypes),
            {NonVarTypeSig, VarTypeSig} = lists:split(NonVarAmount, Types),
            if NonVarTypeSig =:= NonVarTypeArgs ->
                   %% lists:all on an empty list (meaning that the variadic part is empty,
                   %% hence optional) needs to yield true, otherwise this logic is wrong.
		   {RestType, rest} = hd(VarTypeSig),
                   case lists:all(fun(X) -> X =:= RestType end, VarTypeArgs) of
                       true ->
                           ReturnType;
                       false ->
                           erlang:error(io:format("Type of the variadic argument in ~p does not match variadic type of the signature in abstraction ~p~n",
                                        [Arguments, Abstraction]))
                   end;
               true ->
                   erlang:error(io:format("Arguments ~p types do not match the parameters types of the abstraction ~p~n",
                                [Arguments, Abstraction]))
            end;
        _ ->
            erlang:error(io:format("Couldn't type check application with arguments ~p and abstraction ~p~n",
                         [Arguments, Abstraction]))
    end.

typeCheck(Expression) ->
    typeCheckWithEnvironment(prelude(), Expression).

% Lemos: https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Typing_rules
