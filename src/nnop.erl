%%%Android demo.
-module(nnop).
-export([main/0]).

main() ->
    nnop_port:run(2000,2010,["backdoor"],[]),
    hang().

hang() ->
    timer:sleep(100),
    hang().

