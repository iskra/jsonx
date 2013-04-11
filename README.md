
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
                   {person2,  record_info(fields, person2)}], [{format, struct}]).
```

```erlang
1> c(examples).
{ok,examples}

2>  rr(examples).
[person,person2]

3> BabaYaga = #person2{name = <<"BabaYaga">>, age = 118, phone = <<"666-66-66">>}.
#person2{name = <<"BabaYaga">>,age = 118,
         phone = <<"666-66-66">>}

4>  Vasya = #person{name = <<"Vasya">>, age = 18, friends = [BabaYaga]}.
#person{name = <<"Vasya">>,age = 18,
        friends = [#person2{name = <<"BabaYaga">>,age = 118,
                            phone = <<"666-66-66">>}]}

5> Encoder = examples:encoder().
#Fun<jsonx.0.45888425>

6> Decoder = examples:decoder().
#Fun<jsonx.1.21317315>

7> Json = Encoder(BabaYaga).
<<"{\"name\": \"BabaYaga\",\"age\": 118,\"phone\": \"666-66-66\"}">>

8> Decoder(Json).
#person2{name = <<"BabaYaga">>,age = 118,
         phone = <<"666-66-66">>}
9> Json2 = Encoder(Vasya).
<<"{\"name\": \"Vasya\",\"age\": 18,\"friends\": [{\"name\": \"BabaYaga\",\"age\": 118,\"phone\": \"666-66-66\"}]}">>

10> Decoder(Json2).
#person{name = <<"Vasya">>,age = 18,
        friends = [#person2{name = <<"BabaYaga">>,age = 118,
                            phone = <<"666-66-66">>}]}

11> Json3 = <<"[{\"name\": \"BabaYaga\",\"age\": 118,\"phone\": \"666-66-66\"}, {\"record\": \"undefined\", \"strict\": false}]">>.
<<"[{\"name\": \"BabaYaga\",\"age\": 118,\"phone\": \"666-66-66\"}, {\"record\": \"undefined\", \"strict\": false}]">>

12> Decoder(Json3).
{error,undefined_record,64}

13>  NSDecoder = examples:nstrict_decoder().
#Fun<jsonx.2.71844966>

14> JTerm =  NSDecoder(Json3).
[#person2{name = <<"BabaYaga">>,age = 118,
          phone = <<"666-66-66">>},
 [{<<"record">>,<<"undefined">>},{<<"strict">>,false}]]

15> Encoder(JTerm).
<<"[{\"name\": \"BabaYaga\",\"age\": 118,\"phone\": \"666-66-66\"},{\"record\":\"undefined\",\"strict\":false}]">>
```


Decode (JSON -> Erlang)
----------------------

    null             :-> null
    true             :-> true
    false            :-> false
    "string"         :-> <<"binary">>
    [1, 2.3, []]     :-> [1, 2.3, []]
    {"this": "json"} :-> {[{<<"this">>: <<"json">>}]}         %% default eep18
    {"this": "json"} :-> [{<<"this">>: <<"json">>}]           %% optional proplist
    {"this": "json"} :-> {struct, [{<<"this">>: <<"json">>}]} %% optional struct
    JSONObject       :-> #rec{...}                            %% decoder must be predefined

Encode (Erlang -> JSON)
-----------------------

    null                                 :-> null
    true                                 :-> true
    false                                :-> false
    atom                                 :-> "atom"
    <<"str">>                            :-> "str"
    [1, 2.99]                            :-> [1, 2.99]
    {struct, [{<<"this">>: <<"json">>}]} :-> {"this": "json"}
    [{<<"this">>: <<"json">>}]           :-> {"this": "json"}
    {[{<<"this">>: <<"json">>}]}         :-> {"this": "json"}
    {json, IOList}                       :-> `iolist_to_binary(IOList)`  %% include with no validation
    #rec{...}                            :-> JSONObject                  %% encoder must be predefined

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
