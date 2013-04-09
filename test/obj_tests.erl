-module(obj_tests).
-include_lib("eunit/include/eunit.hrl").

%% %% Test encode empty object
enco0_test() ->  {no_match, {}} = jsonx:encode({}).

%% Test encode proplist
encpl0_test() ->  <<"{\"a\":1}">> = jsonx:encode([{a,1}]). 
encpl1_test() ->  <<"{\"a\":{\"b\":2}}">> = jsonx:encode([{a, [{b, 2}]}]).

%% Test encode eep18
enceep0_test() ->  <<"{}">> = jsonx:encode({[]}).
enceep1_test() ->  <<"{\"a\":1}">> = jsonx:encode({[{a,1}]}).
enceep2_test() ->  <<"{\"a\":1,\"b\":2}">>= jsonx:encode({[{a,1},{b,2}]}).

%% Test encode struct
encst0_test() ->  <<"{}">> = jsonx:encode({struct, []}).
encst1_test() ->  <<"{\"a\":1}">> = jsonx:encode({struct, [{a,1}]}).
encst2_test() ->  <<"{\"a\":1,\"b\":2}">>= jsonx:encode({struct, [{a,1},{b,2}]}).

%% Test decode object
decobj0_test() ->
    {[]} = jsonx:decode(<<"{}">>).
decobj1_test() ->
    {[{<<"a">>,1}]} = jsonx:decode(<<"{\"a\": 1}">>).
decobj2_test() ->
    {[{<<"a">>,1}, {<<"XX">>, [true, false]}]} = jsonx:decode(<<"{\"a\": 1, \"XX\": [true, false]}">>).

%% Test decode/2
dec2obj0_test() ->
    {[]} = jsonx:decode(<<"{}">>, []).
dec2obj1_test() ->
    {struct,[]} = jsonx:decode(<<"{}">>, [{format, struct}]).
dec2obj2_test() ->
    {[]} = jsonx:decode(<<"{}">>, [{format, eep18}]).
dec2obj3_test() ->
    [] = jsonx:decode(<<"{}">>, [{format, proplist}]).
dec2obj4_test() ->
    {[]} = jsonx:decode(<<"{}">>, [{format, eep18}, {format, struct}, {format, proplist}]).
