-module(records_examples).
-compile(export_all).

-record(person,  {name, age, friends}).
-record(person2, {name, age, phone}).

encoder1() ->
    jsonx:encoder([{person,   record_info(fields, person)},
                   {person2,  record_info(fields, person2)} ]).

decoder1() ->
    jsonx:decoder([{person,   record_info(fields, person)},
                   {person2,  record_info(fields, person2)}]).

nonstrict_decoder1() ->
    jsonx:decoder([{person,   record_info(fields, person)},
                   {person2,  record_info(fields, person2)}], [{format, proplist}]).
