-module(eroaring_tests).

-include_lib("eunit/include/eunit.hrl").

new0_test() ->
    Bits = eroaring:new(),
    ?assertEqual(0, eroaring:cardinality(Bits)).

new1_test() ->
    ?assertException(error, badarg, eroaring:new(-5)),
    Bits = eroaring:new(1234),
    ?assertEqual(0, eroaring:cardinality(Bits)).

new2_test() ->
    ?assertException(error, badarg, eroaring:new(15, 5)),
    Bits = eroaring:new(1123, 1234),
    ?assertEqual(112, eroaring:cardinality(Bits)).

add_bit_test() ->
    Bits0 = eroaring:new(),
    ?assertEqual(0, eroaring:cardinality(Bits0)),
    Bits1 = eroaring:add(Bits0, 25),
    ?assertEqual(1, eroaring:cardinality(Bits1)).

add_seq_test() ->
    Seq = lists:seq(57, 234),
    Cnt = erlang:length(Seq),
    Bits0 = eroaring:new(),
    ?assertEqual(0, eroaring:cardinality(Bits0)),
    Bits1 = eroaring:add(Bits0, Seq),
    ?assertEqual(Cnt, eroaring:cardinality(Bits1)).

small_serialize_test() ->
    test_serialize(1123, 1234).

large_serialize_test() ->
    test_serialize(1111, 9999999).

test_serialize(Lo, Hi) ->
    Cnt = (Hi - Lo) + 1,
    Bits = eroaring:new(Lo, Hi),
    ?assertEqual(Cnt, eroaring:cardinality(Bits)),
    Bin = eroaring:serialize(Bits),
    Out = eroaring:deserialize(Bin),
    ?assertEqual(Cnt, eroaring:cardinality(Out)),
    ?assertEqual(Bin, eroaring:serialize(Bits)).

