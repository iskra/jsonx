
JSONX is an Erlang library for efficient decode and encode JSON, implemented in Erlang NIFs.
Works with binaries as strings, arrays as lists and it only knows how to decode UTF-8 (and ASCII).

JSONX VERY FAST!
----------------

See benchmark [si14/erl_json_test](https://github.com/si14/erl_json_test) and tests encode records in `/benchmarks/test_encode_records.erl`

JSONX can encode and decode Erlang records!
-------------------------------------------

```erlang
-module(examples).
-compile(export_all).

-record(person, {name, age}).
-record(person2, {name, age, phone}).

encoder() ->
    jsonx:encoder([{person,   record_info(fields, person)},
                   {person2,  record_info(fields, person2)} ]).

decoder() ->
    jsonx:decoder([{person,   record_info(fields, person)},
                   {person2,  record_info(fields, person2)} ]).
```

~~~~~
1> c(examples).
{ok,examples}
2> rr(examples).
[person,person2]
3>  E = examples:encoder().
#Fun<jsonx.0.65737729>
4>  D = examples:decoder().
#Fun<jsonx.1.114288737>
5> R = #person{name = <<"Vasya">>, age = 16}.
#person{name = <<"Vasya">>,age = 16}
6> J = E(R).
<<"{\"name\": \"Vasya\",\"age\": 16}">>
7> D (J).   
#person{name = <<"Vasya">>,age = 16}
~~~~~


Decode (JSON -> Erlang)
----------------------

    null             -> null
    true             -> true
    false            -> false
    "string"         -> <<"binary">>
    [1, 2.3, []]     -> [1, 2.3, []]
    {"this": "json"} -> {[{<<"this">>: <<"json">>}]}         %% default eep18
    {"this": "json"} -> [{<<"this">>: <<"json">>}]           %% optional proplist
    {"this": "json"} -> {struct, [{<<"this">>: <<"json">>}]} %% optional struct
    JSONObject       -> #rec{...}                            %% decoder must be predefined

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
    #rec{...}                            -> JSONObject                  %% encoder must be predefined

INSTALL and DOCUMENTATION
-------------------------

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
