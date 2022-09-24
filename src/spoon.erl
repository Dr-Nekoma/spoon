-module(spoon).

-import(do, [fmap/2]).
-import(prelude, [prelude/0]).
-import(evaluator, [eval/1]).
-import(typer, [typeCheck/1]).

-export([main/1]).

main(Arg) ->
    case file:read_file(Arg) of
        {ok, Binary} ->
            case knife:parse(
                     erlang:binary_to_list(Binary))
            of
                {failure, Message} ->
                    erlang:error(
                        io:format("~p~n", [Message]));
                {success, AST} ->
                    _ = typer:typeCheck(AST),
                    evaluator:eval(AST)
            end;
        {error, _} ->
            erlang:error("Coundn't read file from provided filepath")
    end.
