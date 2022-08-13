-module(prelude).

-export([prelude/0]).

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