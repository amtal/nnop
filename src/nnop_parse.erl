%%% Recursive descent parser of nmap-service-probes file.
%%% 
%%% Taking the fiddly manual approach since I don't trust the nmap format.
%%% Admittedly I haven't looked at how nmap parses it, but I'm betting
%%% it's also manual and the format ad-hoc.
-module(nnop_parse).
-export([main/0, parse_current/0, parse/1]).

main() ->
    io:format("~p~n", [parse_current()]).

%% Deal with most up-to-date file we've got.
parse_current() -> 
    {ok, Full} = file:read_file("priv/nmap-service-probes"),
    parse(Full).

%% Parse nmap-service-probes contents into list of nested tagged tuples.
%%
%% File structure based on http://nmap.org/book/vscan-fileformat.html which 
%% turned out to be accurate and completely lacking in surprises. Didn't see
%% that coming!
-spec parse(binary()) -> [any()].
parse(Full) ->
    [Tok || Tok <- [line(Line) || Line <- tokens(<<"\n">>, Full)], Tok/=skip].


%%% Utilities
token(Sep, Bin) -> binary:split(Bin, [Sep]).
tokens(Sep, Bin) -> binary:split(Bin, [Sep], [global]).
bin2int(Bin) -> list_to_integer(binary_to_list(Bin)).

%%% Detailed parsing


% stimulus generator:
line(<<"Probe ", Probe/binary>>) -> 
    [Protocol,Rest] = token(<<" ">>, Probe),
    [Name,Stimulus] = token(<<" ">>, Rest),
    {probe, Protocol, Name, probe(Stimulus)};
% pattern matches:
line(<<"match ", All/binary>>) -> match(match, All);
line(<<"softmatch ", All/binary>>) -> match(softmatch, All);
% common port usage hints: 
line(<<"ports ", PortList/binary>>) -> ports(ports, PortList);
line(<<"sslports ", PortList/binary>>) -> ports(ssl_ports, PortList);
% frequency hint, higher is less frequent:
line(<<"rarity ", R/binary>>) -> {rarity, bin2int(R)};
% probe timeout in milliseconds:
line(<<"totalwaitms ", Ms/binary>>) -> {total_wait, bin2int(Ms)};
% suggested probes if current probe was unssucessful:
line(<<"fallback ", Probes/binary>>) -> {fallback, tokens(<<",">>, Probes)};
% one-time port scan exclusion for blind printer reasons, heh
line(<<"Exclude ", _M/binary>>) -> skip;
% no interesting stuff in the comments afaik
line(<<"#", _Comment/binary>>) -> skip;
% interestingly, no extraneous whitespace!
line(<<>>) -> skip;
% also quite simple top-level structure :)
line(Unk) -> io:format("unk ~p~n", [Unk]), skip.


%% Test stimulus to send. Very simple C-style escaped content.
%% Supposed to support non-| markers, but haven't seen any yet.
probe(<<"q|", Rest/binary>>) -> 
    % remove the trailling |
    Size = size(Rest) - 1,
    <<Stim:Size/binary, "|">> = Rest,
    Stim;
probe(Unk) -> io:format("probe ~p~n", [Unk]), unk.

%% "Likely" plain or ssl-wrapped ports the service will be seen on.
ports(Type, PortList) ->
    Ports = [port(token(<<"-">>, P)) || P <- tokens(<<",">>, PortList)],
    {Type, Ports}.
% where
    port([Low, High]) -> {range, bin2int(Low), bin2int(High)};
    port([Single]) -> bin2int(Single).

%% Protocol match with a 'main' clause and possibly extra information clauses.
match(Type, All) ->
    [Service,Rest] = token(<<" ">>, All),
    Matches = match_patterns(Rest),
    {Type, Service, Matches}.

%% Handles the format's match/option syntax.
%%
%% Example:
%%   match_patterns(<<"m/foobar/s>>).
%%   [{$m, <<"foobar">>, <<"s">>, <<"asdf">>}]
%%
%% This is the key source of regexen I'm after; may be worth tweaking the format.
-spec match_patterns(binary()) -> 
    [{Type::binary(), Match::binary(), MatchOpt::binary()}].
match_patterns(<<>>) -> [];
match_patterns(<<Bin/binary>>) ->
    {Type,Bin1} = match_type(Bin),
    <<Delim:1/binary, Bin2/binary>> = Bin1,
    [Match,Bin3] = token(Delim, Bin2),
    % suffix is used to specify regex options, and is usually not present
    [MatchOpt,Rest] = case token(<<" ">>, Bin3) of
        [Opt] -> [Opt, <<>>];
        Found -> Found
    end,
    [{Type, Match, [match_opts(O)||O<-binary_to_list(MatchOpt)]}|match_patterns(Rest)].
% where
    match_type(<<"m", Bin/binary>>) -> 
        {match, Bin}; % standard match identifying protocol
    % bunch of special case extra-info stuff:
    match_type(<<"cpe:", Bin/binary>>) -> {cpe, Bin};
    match_type(<<$p, Bin/binary>>) -> {product, Bin};
    match_type(<<$v, Bin/binary>>) -> {version, Bin};
    match_type(<<$i, Bin/binary>>) -> {info, Bin};
    match_type(<<$h, Bin/binary>>) -> {hostname, Bin};
    match_type(<<$o, Bin/binary>>) -> {os, Bin};
    match_type(<<$d, Bin/binary>>) -> {device, Bin}.
    % vscan-fileformat.html claims only 2 options are supported:
    match_opts($s) -> newlines; % newlines in '.' specifier
    match_opts($i) -> any_case; % case insensitive
    % cpe: has its own options:
    match_opts($a) -> cpe_service;
    match_opts($o) -> cpe_os;
    match_opts($h) -> cpe_hardware.

