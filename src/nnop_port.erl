-module(nnop_port).
-export([run/2, run/4, start/2]).

run(Only_, Never_) -> run(2000,2100,Only_,Never_).
run(From, To, Only_, Never_) ->
    Only = [list_to_binary(S)||S<-Only_],
    Never = [list_to_binary(S)||S<-Never_],
    AllProbes = nnop_parse:parse_current(),
    Probes = [{ok,A,R}||{ok,A,R}<-nnop_tree:regexen_gen(AllProbes, Only, Never)],
    [spawn_link(fun()-> start(Probes, N) end) || N<-lists:seq(From,To)].

start(Probes, Port) when is_integer(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, 
                                       {active, false}]),
    accept_loop(Probes, LSock).

accept_loop(Probes, LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Index = crypto:rand_uniform(1, length(Probes)),
    {ok,Answer,_Re} = lists:nth(Index, Probes),
    spawn_link(fun()->conn_loop(Answer, Sock) end),
    accept_loop(Probes, LSock).
    %ok = gen_tcp:close(Sock).

conn_loop(Probes, Sock) ->
    gen_tcp:send(Sock, Probes),
    %io:format("~p tx: ~p~n", [self(), Probes]),
    case gen_tcp:recv(Sock, 0) of
        {ok, Bin} ->
            %io:format("~p rx: ~p~n", [self(), Bin]),
            conn_loop(Probes, Sock);
        {error, closed} ->
            ok
    end.
