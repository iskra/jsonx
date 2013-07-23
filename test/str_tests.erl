-module(str_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test encode atom
enca0_test() -> <<"\"\"">> = jsonx:encode('').
enca1_test() -> <<"\"atom\"">> = jsonx:encode(atom).
enca2_test() -> <<"[true,null,false,\"atom\"]">> = jsonx:encode([true, null, false, atom]).
enca3_test() -> <<"{\"null\":null,\"atom\":\"atom\"}">> = jsonx:encode([{null,null},{atom,atom}]).

%% Test encode binary
encb0_test() -> <<"\"\"">> = jsonx:encode('').
encb1_test() -> <<"\"binary\"">> = jsonx:encode(binary).
encb2_test() -> <<"\"binary\"">> = jsonx:encode('binary').

%% Test encode with escape string
encesc0_test() ->
    <<"\"\\b\\t\\n\\v\\f\\r\\\"'/\\\\\"">> = jsonx:encode(<<8,9,10,11,12,13,34,39,47,92>>).
encesc1_test() ->
    <<"\"\\u0000\\u0007\\u000e\\u001f\\u007f\"">> = jsonx:encode(<<0,7,14,31,127>>).
encesc2_test() ->
    <<"\"\\u0000\\b\\r\\u000e\\u001f\\u007f\"">> = jsonx:encode('\000\b\r\016\037\d').
%% Test encode with validate utf-8 string
encutf0_test() ->
    <<34,192,128,224,128,128,240,128,128,128,34>> = jsonx:encode(<<192, 128, 224, 128, 128, 240, 128, 128, 128>>).
encutf1_test() ->
    {no_match,<<128>>} = jsonx:encode(<<128>>).
encutf2_test() ->
    {no_match,<<191, 128>>} = jsonx:encode(<<191, 128>>).
encutf3_test() ->
    {no_match,<<224, 128>>} = jsonx:encode(<<224, 128>>).
encutf4_test() ->
    {no_match,<<240, 128, 128>>} = jsonx:encode(<<240, 128, 128>>).
encutf5_test() ->
     {no_match,<<248,128,128,128,128>>} = jsonx:encode(<<248, 128, 128, 128, 128>>).


%% Test decode atoms
dectrue_test() ->
    true = jsonx:decode(<<"true">>).
decfalse_test() ->
    false = jsonx:decode(<<"false">>).
decnull_test() ->
    null = jsonx:decode(<<"null">>).

%% Test decode string
decstr0_test() ->
    <<>> = jsonx:decode(<<"\"\"">>).
decstr1_test() ->
    <<"...">> = jsonx:decode(<<"\"...\"">>).
decstr2_test() ->
    <<192, 128, 224, 128, 128, 240, 128, 128, 128>> = jsonx:decode(<<34,192,128,224,128,128,240,128,128,128,34>>).
decstr3_test() ->
    <<"/">> = jsonx:decode(<<"\"\\/\"">>).
decstre0_test() ->
    {error,invalid_string,0} = jsonx:decode(<<34,192,34>>).
decstre01_test() ->
    {error,invalid_string,0} = jsonx:decode(<<34,0,34>>).
decstre02_test() ->
    {error,invalid_string,0} = jsonx:decode(<<34,127,34>>).
decstre1_test() ->
    {error,invalid_string,0} = jsonx:decode(<<34,224,128,34>>).
decstre2_test() ->
    {error,invalid_string,0} = jsonx:decode(<<34,240,128,128,34>>).
decstre3_test() ->
    {error,invalid_string,0} = jsonx:decode(<<34,255,34>>).
decstresc1_test() ->
    [<<".\b.\t.\v.\f.\r.\".\\.">>] = jsonx:decode(<<"[\".\\b.\\t.\\v.\\f.\\r.\\\".\\\\.\"]">>).
decstresc2_test() ->
    <<9,192,191,224,191,191,240,191,191,128,10>> =jsonx:decode(<<34,"\\t",192,191,224,191,191,240,191,191,128,"\\n",34 >>).
decstrescerr1_test() ->
    {error,invalid_string,0} = jsonx:decode(<<34,"\\t",192,34 >>).
decstrescerr2_test() ->
    {error,invalid_string,0} = jsonx:decode(<<34,"\\n",224,128,34 >>).
decstrescerr3_test() ->
    {error,invalid_string,0} = jsonx:decode(<<34,"\\r",240,128,128,34 >>).
decstrescu1_test() ->
    <<"Dn..">> = jsonx:decode(<<34,"\\u0044\\u006e..",34>>).
decstrescu2_test() ->
    <<194,128>> = jsonx:decode(<<34,"\\u0080",34>>).
