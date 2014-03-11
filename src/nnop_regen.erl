-module(nnop_regen).
-export([gen/1]).

%  T=nnop_parse:parse_current(),io:format("~W~n",[(nnop_tree:regexen_gen(T)), 100]).

%% Generate output matching an input PCRE regex.
%-spec gen(binary())->any(). % what do you call a binlist again?


gen(<<"^220.*Microsoft FTP Service \\(Version (\\d[^)]+)">>) -> ok;
gen(<<"^220-\\r\\n220 ([-.\\w]+) FTP server \\(Version ([-.+\\w()]+)\\) ready\\.\\r\\n$">>) -> ok;
gen(<<"^220 ([-.\\w]+) FTP server \\(Revision ([\\d.]+) Version wuftpd-([-.+\\w()]+) [^)]*\\) ready\\.\\r\\n$">>) -> ok;
gen(Val) -> 
    %io:format("Generating: ~s~n",[Val]),
    try begin
        Gen = gen(Val, <<>>),
        %io:format("out: ~p~n", [Gen]),
        %io:format("out: ~p~n", [iolist_to_binary(Gen)]),
        %io:format("san: ~p~n", [re:run(iolist_to_binary(Gen), Val)]),
        case re:run(iolist_to_binary(Gen), Val) of 
            nomatch -> {bad, iolist_to_binary(Gen), Val};
            {match, _} -> {ok, iolist_to_binary(Gen), Val}
        end
    end catch
        Type:E -> 
            io:format("exception: ~p~n", [E]),
            {fail, Type, E}
    end.

gen(<<>>,Last) -> Last; % done
gen(<<$^, Re/binary>>, Last) -> gen(Re, Last);
gen(<<$$>>,Last) -> gen(<<>>, Last);
gen(Any, Last) -> 
    case lit(Any) of
        {repeat, Count, Re} -> gen(Re, lists:duplicate(Count, Last));
        {Lit,Re} -> [Last,gen(Re,Lit)]
    end.

lit(<<$\\, $x, Hi, Lo, Re/binary>>) -> {hex(Hi, Lo), Re}; 
lit(<<$\\, Char, Re/binary>>) -> {spec_char(Char), Re};
lit(<<C, Re/binary>>) when C>=$0, C=<$9;
                           C>=$A, C=<$Z;
                           C>=$a, C=<$z;
                           C==$ ; C==$-; C==$<; C==$>; C==$#;
                           C==$`; C==$_; C==$=; C==$%; C==$@;
                           C==$'; C==$/; C==$!; C==$:; C==$&;
                           C==$,; C==$~; C==$^; C==$;; C==$"
                           -> {C, Re};
lit(<<${, Re/binary>>) -> min_max(Re);
lit(<<$[, Re/binary>>) -> class(Re);
lit(<<$(, Re/binary>>) -> 
    Re2 = case Re of
        <<"?:",X/binary>> -> X; % captures worth using?
        _ -> Re
    end,
    [Group,Rest] = token($), Re2),
    {gen(Group,<<>>), Rest};
lit(<<$|, _/binary>>) -> {<<>>, <<>>}; % keeping it simple
lit(<<$., Re/binary>>) -> {$A, Re}; % keeping output simple
lit(<<$*, Re/binary>>) -> {repeat, 0, Re}; % keeping output small
lit(<<$?, Re/binary>>) -> {repeat, 0, Re}; % keeping output small
lit(<<$+, Re/binary>>) -> {repeat, 1, Re}. % keeping output small
% where
    spec_char($r) -> <<"\r">>;
    spec_char($n) -> <<"\n">>;
    spec_char($.) -> $.;
    spec_char($*) -> $*;
    spec_char($+) -> $+;
    spec_char($?) -> $?;
    spec_char($$) -> $$;
    spec_char($^) -> $^;
    spec_char($|) -> $|;
    spec_char($\\) -> $\\;
    spec_char($[) -> $[;
    spec_char($]) -> $];
    spec_char($() -> $(;
    spec_char($)) -> $);
    spec_char(${) -> ${;
    spec_char($}) -> $};
    spec_char($&) -> $&; % the heck does this do?
    spec_char($@) -> $@; % the heck does this do?
    spec_char($ ) -> $ ; % the heck does this do?
    spec_char($d) -> $0; % simple
    spec_char($D) -> $A; % simple
    spec_char($w) -> $A; % simple
    spec_char($W) -> $ ; % simple
    spec_char($s) -> $ ; % simple
    spec_char($S) -> $A; % simple
    spec_char($") -> $"; % I don't think you need to escape these?
    spec_char($t) -> 9;
    spec_char($0) -> 0.

min_max(<<>>) -> {${, <<>>}; % weird?
min_max(M) ->
    [Minmax,Rest] = token($}, M),
    Length = int(case token($,, Minmax) of
        [Min,_Max] -> Min;
        [Count] -> Count
    end),
    {repeat, Length, Rest}.

class(<<$^, $), Rest/binary>>) ->
    [_,Re] = token($],Rest),
    {$A,Re}; % dirty hack
class(<<$^, Rest/binary>>) ->
    {Val,Re} = class(Rest),
    {case iolist_to_binary([Val]) of
        <<$A>> -> $B;
        <<_>> -> $A
    end, Re};
class(<<>>) -> <<>>;
class(C) ->
    %io:format("#~p~n", [C]),
    [Class,Rest] = token($],C),
    Val = case Class of
        <<From,$-,_To,_Others/binary>> -> From;
        <<$.,_/binary>> -> $.; % annoying edge case? is this right?
        <<$+,_/binary>> -> $+; % annoying edge case? is this right?
        <<$-,_/binary>> -> $-; % annoying edge case? is this right?
        <<$*,_/binary>> -> $*; % annoying edge case? is this right?
        <<$?,_/binary>> -> $?; % annoying edge case? is this right?
        <<>> -> <<>>;
        <<Bin/binary>> -> 
            {V,_Re} = lit(Bin), % get 1st
            V
    end,
    {Val, Rest}.


%% Utility.
token(Sep, Bin) -> 
    case binary:split(Bin, [<<Sep>>]) of
        [<<>>,B] -> [<<>>,B];
        [A,B] -> 
            % sanity check match didn't capture a \sep
            case binary:at(A,size(A) - 1) of
                $\\ ->
                    % escaped separator, retry
                    case token(Sep, B) of 
                        [X,Y] ->
                            [iolist_to_binary([A,Sep,X]), Y];
                        [_] -> % real separator not found
                            iolist_to_binary([A,Sep,B])
                    end;
                _ -> [A,B]
            end;
        Other -> Other
    end.
int(Bin) -> 
    {Int,[]} = string:to_integer(binary_to_list(Bin)),
    Int.
hex(Hi, Lo) -> hex_norm(Hi)*16 + hex_norm(Lo).
% where
    hex_norm(N) when N>=$0, N=<$9 -> N-$0;
    hex_norm(N) when N>=$a, N=<$f -> N-$a+10;
    hex_norm(N) when N>=$A, N=<$F -> N-$A+10.

