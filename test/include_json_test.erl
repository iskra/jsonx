-module(include_json_test).
-include_lib("eunit/include/eunit.hrl").

%% Test encode {json, iolist}
encjs0_test() -> <<>> = jsonx:encode({json, []}).
encjs1_test() -> <<>> = jsonx:encode({json, <<"">>}).
encjs2_test() -> <<"[22,23,24]">> = jsonx:encode({json, <<"[22,23,24]">>}).
