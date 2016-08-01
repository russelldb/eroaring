-module(eroaring_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ?assertException(error, badarg, eroaring:new(foo)).
