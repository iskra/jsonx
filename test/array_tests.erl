-module(array_tests).
-include_lib("eunit/include/eunit.hrl").
  
%% Test encode list
encl0_test() ->  <<"[]">> = jsonx:encode([]).
encl1_test() ->  <<"[[[]]]">> = jsonx:encode([[[]]]).
encl2_test() ->  <<"[true,1,1.1,\"bin\"]">> = jsonx:encode([true, 1, 1.1, <<"bin">>]).

%% Test decode array
decarr0_test() ->
    [] = jsonx:decode(<<"[]">>).
decarr1_test() ->
    [[],[[]],[1],[true,false]] = jsonx:decode(<<" [[], [[]], [1], [true, false]] ">>).
