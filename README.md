
JSONX is an Erlang library for efficient decode and encode JSON, written in C.
Works with binaries as strings, arrays as lists and it only knows how to decode UTF-8 (and ASCII).

JSONX VERY FAST!
----------------

See benchmark [si14/erl_json_test](https://github.com/si14/erl_json_test) and tests encode records in `/benchmarks/test_encode_records.erl`


Decode (JSON -> Erlang)
----------------------

    null             -> null
    true             -> true
    false            -> false
    "string"         -> <<"binary">>
    [1, 2.3, []]     ->  [1, 2.3, []]
    {"this": "json"} -> {[{<<"this">>: <<"json">>}]}         %% default eep18
    {"this": "json"} -> [{<<"this">>: <<"json">>}]           %% optional proplist
    {"this": "json"} -> {struct, [{<<"this">>: <<"json">>}]} %% optional struct

Encode (Erlang -> JSON)
-----------------------

    null                                 -> null
    true                                 -> true
    false                                -> false
    atom                                 -> "atom"
    <<"str">>                            -> "str"
    [1, 2.99]                            -> [1, 2.99]
    {struct, [{<<"this">>: <<"json">>}]} -> {"this": "json"}
    [{<<"this">>: <<"json">>}]           -> {"this": "json"}
    {[{<<"this">>: <<"json">>}]}         -> {"this": "json"}
    {json, IOList}                       -> `iolist_to_binary(IOList)`  %% include with no validation
    -record(...)                         -> See bellow

INSTALL and DOC
---------------

* cd jsonx
* make
* make doc
* firefox doc/index.html&

Examples encode json
--------------------

~~~~~
1>  jsonx:encode([1, 2.3, true, false, null, atom, <<"string">>, []]).
<<"[1,2.3,true,false,null,\"atom\",\"string\",[]]">>

%% Object as proplist
2>  jsonx:encode( [{name, <<"Ivan">>}, {age, 33}, {phones, [3332211, 4443322]}] ).
<<"{\"name\":\"Ivan\",\"age\":33,\"phones\":[3332211,4443322]}">>

%% Object as struct
3>  jsonx:encode( {struct, [{name, <<"Ivan">>}, {age, 33}, {phones, [3332211, 4443322]}]} ).
<<"{\"name\":\"Ivan\",\"age\":33,\"phones\":[3332211,4443322]}">>

%% Object as eep18 propsal
4>  jsonx:encode( {[{name, <<"Ivan">>}, {age, 33}, {phones, [3332211, 4443322]}]} ).
<<"{\"name\":\"Ivan\",\"age\":33,\"phones\":[3332211,4443322]}">>
~~~~~

Examples decode json
--------------------

~~~~~
1> jsonx:decode(<<"{\"name\":\"Ivan\",\"age\":33,\"phones\":[3332211,4443322]}">>).
{[{<<"name">>,<<"Ivan">>},
  {<<"age">>,33},
  {<<"phones">>,[3332211,4443322]}]}

2> jsonx:decode(<<"{\"name\":\"Ivan\",\"age\":33,\"phones\":[3332211,4443322]}">>, [{format, eep18}]).
{[{<<"name">>,<<"Ivan">>},
  {<<"age">>,33},
  {<<"phones">>,[3332211,4443322]}]}

3> jsonx:decode(<<"{\"name\":\"Ivan\",\"age\":33,\"phones\":[3332211,4443322]}">>, [{format, proplist}]).
[{<<"name">>,<<"Ivan">>},
 {<<"age">>,33},
 {<<"phones">>,[3332211,4443322]}]

4> jsonx:decode(<<"{\"name\":\"Ivan\",\"age\":33,\"phones\":[3332211,4443322]}">>, [{format, struct}]). 
{struct,[{<<"name">>,<<"Ivan">>},
         {<<"age">>,33},
         {<<"phones">>,[3332211,4443322]}]}
~~~~~

Examples encode record to json
------------------------------

```erlang
-module(x).
-export([encode_records_test/0]).
-record(none, {}).
-record(person, {name :: binary(), age :: number()}).
-record(person2, {name, age, phone}).

encode_records_test() ->
    jsonx:encoder([{none,     record_info(fields, none)},
                   {person,   record_info(fields, person)},
                   {person2,  record_info(fields, person2)} ]).
```

~~~~~
10> c(x).
 {ok,x}
                                                              
11> F = x:encode_records_test().
 #Fun<jsonx.0.126959313>
                                              
12> F([ {none}, {person,<<"IvanDurak">>,16}, {person2, <<"BabaYaga">>, 116, 6666666} ]).
 <<"[{},{\"name\": \"IvanDurak\",\"age\": 16},{\"name\": \"BabaYaga\",\"age\": 116,\"phone\": 6666666}]">>
~~~~~
