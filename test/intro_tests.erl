-module(intro_tests).
-include_lib("eunit/include/eunit.hrl").


%% Test trailing data
trail_test() ->
 {error,trailing_data,4} = jsonx:decode(<<"123 ,">>).

%% Test stack
stack_test() ->
    L = lists:seq(0,99),
    LL = L ++ L,
    LL = jsonx:decode(jsonx:encode(LL)).

