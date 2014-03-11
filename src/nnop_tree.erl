%%% Simple analysis of probe tree.
-module(nnop_tree).
-export([simplify/1, targets/1, regexen/1, regexen/3, regexen_gen/3, main/0]).

%% Not that many probe types. Most of them are high rarity.
%%
%% High rarity have very specific ports.
%%
%% fallback is always to GetRequest
simplify(Ops) ->
    [SimOp || SimOp<-[sim(Op) || Op<-Ops], SimOp/=skip].
% where
    sim({probe,Prot,Name,_}) -> {probe_type,Prot,Name};
    sim({match,_Name,_Matches}) -> skip;
    sim({sslports,_}) -> skip;
    sim(Op) -> Op.

%% Rough estimate of how many things we can get.
%%
%% There's a pretty limited number (<100) of top-level softmatches.
%%
%% Actual matches are where the real interesting structure is. Eg, 'telnet' is
%% a massive category. So while there's a couple of hundred of unique matches,
%% actual number of services... *grep-wc hackery* is...
%%
%%  grep -v '^#' priv/nmap-service-probes  | grep match | wc -l
%%  8918
%%
%% Well, I don't know how many I can fit into a typical scan. Does nmap ident
%% every open port if you do -a? Will need testing. Should definitely be able
%% to crank out some interesting results, though.
%%
%% Oh man, there's matches for TCP services to JTAG debuggers :D
targets(Ops) ->
    [SimOp || SimOp<-[tar(Op) || Op<-Ops], SimOp/=skip].
% where
    % probes with no matches but output may give interesting results as well
    tar({probe,_,Name,_}) -> {probe, Name}; 
    % softmatches are top-level things with a unique hardmatch beneath them
    % afaik
    tar({softmatch,Name,_Matches}) -> {softmatch, Name};
    tar({match,Name,_Matches}) -> {match, Name};
    tar(_) -> skip.

regexen(Ops) -> regexen(Ops, [], []).

regexen(Ops, Only, Never) ->
    Res = [SimOp||SimOp<-[reg(Op, Only, Never) || Op<-Ops], SimOp/=skip],
    %file:write_file("test.txt", 
    %    lists:concat([[SimOp, <<"\n">>] || SimOp<-Res])),
    Res.
% where
    reg({Type,Name,Matches}, [], Never) when Type==match; Type==softmatch ->
        case lists:member(Name, Never) of 
            true -> [];
            false -> [M||M<-[match(M)||M<-Matches], M/=skip]
        end;
    reg({Type,Name,Matches}, Only, _) when Type==match; Type==softmatch -> 
        case lists:member(Name, Only) of 
            false -> [];
            true -> [M||M<-[match(M)||M<-Matches], M/=skip]
        end;
    reg(_, _, _) -> skip.
    match({match,Re,_Opts}) -> Re;
    match({softmatch,Re,_Opts}) -> Re; % wrong?
    match(_) -> skip.

regexen_gen(Ops, Only, Never) ->
    [nnop_regen:gen(M)||[M]<-regexen(Ops, Only, Never)].

main() ->
    _T=nnop_parse:parse_current(), 
    %io:format("~W~n", [(catch nnop_tree:regexen_gen(T)), 100]).
    ok.

