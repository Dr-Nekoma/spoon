-module(prelude).

-export([prelude/0]).

boolLessThan() ->
    {"<", genNativeComparisonFunc(fun erlang:'<'/2)}.

boolGreaterThan() ->
    {">", genNativeComparisonFunc(fun erlang:'>'/2)}.

boolEqual() ->
    {"=:=", genNativeComparisonFunc(fun erlang:'=:='/2)}.

boolGreaterEqualThan() ->
    {">=", genNativeComparisonFunc(fun erlang:'>='/2)}.

boolLessEqualThan() ->
    {"<=", genNativeComparisonFunc(fun erlang:'=<'/2)}.

boolAnd() ->
    {"and", genNativeLogicalFunc(fun erlang:'and'/2, true)}.

boolOr() ->
    {"or", genNativeLogicalFunc(fun erlang:'or'/2, false)}.

boolNot() ->
    {"not", genNativeBooleanUnaryFunc(fun erlang:'not'/1)}.

arithAdd() ->
    {"+", genNativeArithmeticFunc(fun erlang:'+'/2)}.

arithSubtract() ->
    {"-", genNativeArithmeticFunc(fun erlang:'-'/2)}.

arithMultiplication() ->
    {"*", genNativeArithmeticFunc(fun erlang:'*'/2)}.

arithDivision() ->
    {"/",
     genNativeArithmeticFunc(fun (_, 0) ->
                                     erlang:error("Division by zero is not allowed");
                                 (A, B) ->
                                     erlang:round(A / B)
                             end)}.

prelude() ->
    maps:from_list([arithAdd(),
                    arithSubtract(),
                    arithMultiplication(),
                    arithDivision(),
                    boolAnd(),
                    boolOr(),
                    boolNot(),
                    boolGreaterEqualThan(),
                    boolLessEqualThan(),
                    boolEqual(),
                    boolLessThan(),
                    boolGreaterThan()]).

genNativeBooleanUnaryFunc(Operator) ->
    {nativeFunc,
     {function, notVariadic, [boolean], boolean},
     fun (X) when length(X) =:= 1 ->
             [{literal, {Type, Y}}] = X,
             {ok, {literal, {Type, Operator(Y)}}};
         (X) when length(X) > 1 ->
             erlang:error(io:format("Unary function does not accept multiple parameters as in ~p~n", [X]))
     end}.

genNativeArithmeticFunc(ArithmeticOperator) ->
    Function =
        fun(Element, Accum) ->
           case {Accum, Element} of
               {{ok, {literal, {integer, Acc}}}, {literal, {integer, X}}} ->
                   {ok, {literal, {integer, ArithmeticOperator(Acc, X)}}};
	       Error ->
                   erlang:error(io:format("Could not operate on ~p since it's not a number~n", [Error]))
           end
        end,
    {nativeFunc,
     {function, variadic, [integer], integer},
     fun(List) ->
        [Head | Tail] = lists:flatten(List),
        lists:foldl(Function, {ok, Head}, Tail)
     end}.

genNativeComparisonFunc(Operator) ->
    {nativeFunc,
     {function, notVariadic, [integer, integer], boolean},
     fun ([{literal, {integer, Left}}, {literal, {integer, Right}}]) ->
             {ok, {literal, {boolean, Operator(Left, Right)}}};
         (Error) ->
	     erlang:error(io:format("Could not operate on ~p since it's not a boolean~n", [Error]))
     end}.

genNativeLogicalFunc(BoolOperator, MEmpty) ->
    Function =
        fun(Element, Accum) ->
           case {Accum, Element} of
               {{ok, {literal, {boolean, Acc}}}, {literal, {boolean, X}}} ->
                   {ok, {literal, {boolean, BoolOperator(Acc, X)}}};
               Error ->
                   erlang:error(io:format("Could not operate on ~p since it's not a boolean~n", [Error]))
           end
        end,
    {nativeFunc,
     {function, variadic, [boolean], boolean},
     fun(List) -> lists:foldl(Function, {ok, {literal, {boolean, MEmpty}}}, lists:flatten(List)) end}.
