-module(map_tests).
-include_lib("eunit/include/eunit.hrl").
-ifndef(JSONX_NO_MAPS).
%% Test encode map
encl0_test() -> <<"{}">> = jsonx:encode(#{}).
encl1_test() -> <<"{}">> = jsonx:encode(#{}).
encl2_test() -> <<"{\"a\":{\"b\":{\"c\":3}}}">> = jsonx:encode(#{a=>#{b=>#{c=>3}}}).
encl3_test() -> <<"{\"a\":2,\"b\":3,\"c\":4,\"d\":5}">> = jsonx:encode(#{a=>2, b=>3, c=>4, d=>5}). 

%%% Test decode map
decarr0_test() ->
	{map, #{}} = jsonx:decode(<<"{}">>, [{format, map}]).
decarr1_test() ->
	{map,#{<<"a">> := 2, <<"b">> := 3, <<"c">> := 4, <<"d">> := 5}} =
	jsonx:decode(<<"{\"a\":2,\"b\":3,\"c\":4,\"d\":5}">>, [{format, map}]).
-endif.
