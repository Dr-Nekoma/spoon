-module(typer).

-export([typeCheck/2]).

%% {literal, {integer, 1}}

typeCheck(_, {literal, {Type, _}}) ->
    Type;
typeCheck(Environment, {condition, Case, Then, Else}) ->
    typeCheckCondition(Environment, Case, Then, Else);
typeCheck(Environment, {abstraction, Parameters, Body}) ->
    typeCheckAbstraction(Environment, Parameters, Body);
typeCheck(Environment, {application, Abstraction, Arguments}) ->
    typeCheckApplication(Environment, Abstraction, Arguments).

typeCheckCondition(Environment, Case, Then, Else) ->
    TypeCase = typeCheck(Environment, Case),
    TypeThen = typeCheck(Environment, Then),
    TypeElse = typeCheck(Environment, Else),
    case {TypeThen =:= TypeElse, TypeCase =:= boolean} of
        {_, false} -> erlang:error("Condition must be a boolean and found ~p~n", [TypeCase]);
        {false, _} -> erlang:error("Else branch of type ~p does not conform with expected type ~p of branch Then~n.", [TypeElse, TypeThen]);
        _ -> TypeThen
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
    UniqueList = uniq(lists:map(fun ({X, _}) -> X end, Labels)),
    Types = lists:map(fun ({_, X}) -> X end, Labels),
    if 
        length(UniqueList) =:= length(Labels) ->
            Function = 
                    fun({Name, Type}, Accum) ->
                        maps:put(Name, Type, Accum) 
                    end,
            NewEnvironment = lists:foldl(Function, Environment, Labels),
            ReturnType = typeCheck(NewEnvironment, Body),
            {function, VariadicInfo, Types, ReturnType};
        true ->
            erlang:error("There are duplicated names in abstraction ~p~n", Labels)
    
    end.

typeCheckApplication(Environment, Abstraction, Arguments) -> ok.