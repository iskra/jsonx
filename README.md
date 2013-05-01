
JSONX is an Erlang library for efficient JSON decoding and encoding, implemented in Erlang NIFs.
Works with binaries as strings, arrays as lists and only knows how to decode UTF-8 (and ASCII).

JSONX IS VERY FAST!
------------------

Check out a benchmark [si14/erl_json_test](https://github.com/si14/erl_json_test) or
[davisp/erljson_bench](https://github.com/davisp/erljson_bench) and
record encoding tests in `/test/bench_encode_records.erl`

INSTALLATION and DOCUMENTATION
------------------------------

* cd jsonx
* make
* make doc
* firefox doc/index.html&

JSONX can encode and decode Erlang records!
-------------------------------------------

```erlang
-module(record_example).
-compile(export_all).

-record(person,  {name, age, friends}).
-record(person2, {name, age, phone}).

encoder() ->
    jsonx:encoder1([{person,   record_info(fields, person)},
                   {person2,  record_info(fields, person2)} ]).

decoder() ->
    jsonx:decoder1([{person,   record_info(fields, person)},
                   {person2,  record_info(fields, person2)}]).

nonstrict_decoder1() ->
    jsonx:decoder([{person,   record_info(fields, person)},
                   {person2,  record_info(fields, person2)}],
		  [{format, proplist}]).
```

```erlang
1> c(records_examples).
{ok,records_examples}

2>  rr(record_examples).
[person,person2]

3> BabaYaga = #person2{name = <<"BabaYaga">>, age = 118, phone = <<"666-66-66">>}.
#person2{name = <<"BabaYaga">>,age = 118,
         phone = <<"666-66-66">>}

4>  Vasya = #person{name = <<"Vasya">>, age = 18, friends = [BabaYaga]}.
#person{name = <<"Vasya">>,age = 18,
        friends = [#person2{name = <<"BabaYaga">>,age = 118,
                            phone = <<"666-66-66">>}]}

5> Encoder = examples:encoder1().
#Fun<jsonx.0.45888425>

6> Decoder = examples:decoder1().
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

13>  NonStrictDecoder = examples:nonstrict_decoder1().
#Fun<jsonx.2.71844966>

14> JTerm =  NonStrictDecoder(Json3).
[#person2{name = <<"BabaYaga">>,age = 118,
          phone = <<"666-66-66">>},
 [{<<"record">>,<<"undefined">>},{<<"strict">>,false}]]

15> Encoder(JTerm).
<<"[{\"name\": \"BabaYaga\",\"age\": 118,\"phone\": \"666-66-66\"},{\"record\":\"undefined\",\"strict\":false}]">>
```


Examples encoding JSON
----------------------

```erlang
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
```

Examples decoding JSON
----------------------

```erlang
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
```

Example streaming parse
-----------------------

More example see `examples/stream_example.erl` .

```erlang
1> D = jstream:new_decoder(<<"{\"key1\": \"val1\",\n">>).
<<>>

2> jstream:get_event(D).
start_map

3> jstream:get_event(D).
{map_key,<<"key1">>}

4> jstream:get_event(D).
<<"val1">>

5> jstream:get_event(D).
parse_buf

6> ok = jstream:update_decoder(D, <<"\"key2\": \"val2\"}\n">>).
ok

7> jstream:get_event(D).
{map_key,<<"key2">>}

8> jstream:get_event(D).
<<"val2">>

9> jstream:get_event(D).
end_map

10> jstream:get_event(D).
{parse_end,<<>>}

```

Mapping (JSON -> Erlang)
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

Mapping (Erlang -> JSON)
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
