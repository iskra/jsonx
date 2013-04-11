-module(examples).
-compile(export_all).

-record(person,  {name, age, friends}).
-record(person2, {name, age, phone}).

encoder() ->
    jsonx:encoder([{person,   record_info(fields, person)},
                   {person2,  record_info(fields, person2)} ]).

decoder() ->
    jsonx:decoder([{person,   record_info(fields, person)},
                   {person2,  record_info(fields, person2)}]).

nstrict_decoder() ->
    jsonx:nonstrict_decoder([{person,   record_info(fields, person)},
                   {person2,  record_info(fields, person2)}], [{format, proplist}]).
