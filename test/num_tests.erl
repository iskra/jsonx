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

%% Test decode numerics to decimal
decnum_decimal_integer_test() ->
    123 = jsonx:decode(<<"123">>, [{number_format, decimal}]).
decnum_decimal_zero_test() ->
    {0, 0, -1} = jsonx:decode(<<"0.0">>, [{number_format, decimal}]).
decnum_decimal_frac_exp_test() ->
    {1,12,5} = jsonx:decode(<<"-1.2e6">>, [{number_format, decimal}]).
decnum_decimal_frac_neg_exp_test() ->
    {1,12,-4} = jsonx:decode(<<"-1.2e-3">>, [{number_format, decimal}]).
decnum_decimal_frac_neg_exp_case_test() ->
    {1,12,-4} = jsonx:decode(<<"-1.2E-3">>, [{number_format, decimal}]).
decnum_decimal_frac_test() ->
    {0,12,-1} = jsonx:decode(<<"1.2">>, [{number_format, decimal}]).
decnum_decimal_exp_test() ->
    {0,1,2} = jsonx:decode(<<"1e2">>, [{number_format, decimal}]).
