-module(num_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test encode numeric
enc1_test() ->   <<"1">> = jsonx:encode(1).
enc2_test() ->   <<"-1">> = jsonx:encode(-1).
enc3_test() ->   <<"123456789012345">> = jsonx:encode(123456789012345).
enc4_test() ->   <<"-123456789012345">> = jsonx:encode(-123456789012345).
enc5_test() ->   <<"-1">> = jsonx:encode(-1.0).
enc6_test() ->   <<"-1.1">> = jsonx:encode(-1.1).
enc7_test() ->   <<"11.11">> = jsonx:encode(11.11).
enc8_test() ->   <<"11">> = jsonx:encode(11.00).
enc9_test() ->   <<"65">> = jsonx:encode($A).

%% Test decode numerics
decnum0_test() ->
    0 = jsonx:decode(<<"0">>).
decnumb_test() ->
    1234567890123456789 = jsonx:decode(<<"1234567890123456789">>).
decnumm_test() ->
    -123 = jsonx:decode(<<"-123">>).
decnum00_test() ->
    0.0 = jsonx:decode(<<"0.0">>).
decnum1_test() ->
    -0.0012 = jsonx:decode(<<"-1.2e-3">>).
