-module(encode_records_test).
-include_lib("eunit/include/eunit.hrl").

%% Tests encode records
encrec0_test() ->
    F = jsonx:encoder([ {none, []}, {person, [name, age]}, {person2, [name, age, phone]} ]),
    <<"[{},{\"name\": \"IvanDurak\",\"age\": 16},{\"name\": \"BabaYaga\",\"age\": 116,\"phone\": 6666666}]">>
	=  F([ {none}, {person,<<"IvanDurak">>,16}, {person2, <<"BabaYaga">>, 116, 6666666} ]).

-record(none, {}).
-record(person, {name :: binary(), age :: number()}).
-record(person2, {name, age, phone}).
-record(rec, {a, b}).

encrec1_test() ->
    F = jsonx:encoder([ {none, record_info(fields, none)}, {person,  record_info(fields, person)}, {person2,  record_info(fields, person2)} ]),
    <<"[{},{\"name\": \"IvanDurak\",\"age\": 16},{\"name\": \"BabaYaga\",\"age\": 116,\"phone\": 6666666}]">>
	=  F([ {none}, {person,<<"IvanDurak">>,16}, {person2, <<"BabaYaga">>, 116, 6666666} ]).

encrec2_test() ->
    F = jsonx:encoder([ {rec, record_info(fields, rec)} ]),
    <<"{\"a\": \"x\",\"b\": {\"a\": {\"a\": \"x\",\"b\": \"y\"},\"b\": \"w\"}}">> = F({rec, x, {rec, {rec, x, y}, w}}).



encrec3_test() ->
    %%F = jsonx:encoder([ {spam, [a, b, c]} ], [{ignore,null}]),
    F = jsonx:encoder([ {spam, [a, b, c]} ], [{ignore, [null, undefined]}]),
    <<"{}">> = F({spam, null, null, null}),
    <<"{\"a\": \"nall\"}">> = F({spam, nall, null, undefined}).
