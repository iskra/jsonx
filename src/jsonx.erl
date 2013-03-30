%% @copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

%% @doc JSONX is an Erlang library for efficient decode and encode JSON, written in C.
%%      Works with binaries as strings, arrays as lists and it only knows how to decode UTF-8 (and ASCII).
%%
%%      <h3>Decode (json -> erlang)</h3>
%%      <ul>
%%       <li>null   -> atom null</li>
%%       <li>true   -> atom true</li>
%%       <li>false  -> atom false</li>
%%       <li>string -> binary</li>
%%       <li>number -> number</li>
%%       <li>array  -> list</li>
%%       <li>object -> {struct, PropList}, optional eep18 or proplist.</li>
%%      </ul>
%%      <h3>Encode (erlang -> json)</h3>
%%      <ul>
%%       <li>atom null 	      -> null</li>
%%       <li>atom true 	      -> true</li>
%%       <li>atom true 	      -> false</li>
%%       <li>any other atom     -> string</li>
%%       <li>binary             -> string</li>
%%       <li>number             -> number</li>
%%       <li>{struct, PropList} -> object</li>
%%       <li>{PropList}         -> object</li>
%%       <li>PropList           -> object</li>
%%       <li>{json, IOList}     -> include IOList with no validation</li>
%%      </ul>

-module(jsonx).
-export([decode/1, decode/2, encode/1, encoder/1]).
-on_load(init/0).
-define(LIBNAME, jsonx).
-define(APPNAME, jsonx).

%%@doc Encode JSON.
-spec encode(JSON_TERM) -> JSON when
      JSON      :: binary(),
      JSON_TERM :: any().
encode(_) ->
    not_loaded(?LINE).


%%@doc Encode JSON.
-spec encoder(RECORDS_DESC) -> ENCODER when
      RECORDS_DESC :: [{tag, [names]}],
      ENCODER      :: function().
%% %% Records descriptions for NIF
%% {
%%   Rcnt                                 %% Records count
%%  ,Fcnt                                 %% Counter all fields in records
%%  ,Records = [{Tag, Fields_off, Arity}] %% List of records tag, position and length fields
%%  ,Fields  = [{Name_off, Size}]         %% List of position and size fields names in binary storage
%%  ,Binsz                                  %% Binary data size
%%  ,Bin                                  %% Binary storage for names of fields, format - <,"name": >
%% }
encoder(R) ->
    {Rcnt, Fcnt, Binsz, Rs, Fs, Bin} = inspect_records(R),
    Resource = make_records_resource(Rcnt, Fcnt, Rs, Fs, Binsz, Bin),
    fun(JSON_TERM) -> encode(JSON_TERM, Resource) end.

make_records_resource(_Rcnt, _Fcnt, _Rs, _Fs, _Binsz, _Bin) ->
    not_loaded(?LINE).
encode(_JSON_TERM, _Resource) ->
    not_loaded(?LINE).
inspect_records(T) ->
    {Rcnt, Fcnt, Rs, {Fs, Blen, Bins}} = records(T),
    {Rcnt, Fcnt, Blen, lists:reverse(Rs), lists:reverse(Fs),
     iolist_to_binary(lists:reverse(Bins))}.
records(Rs) ->
    records_(Rs, {_Rcnt = 0, _OffF = 0, _Rs = [],
	    {_Fields = [], _OffB = 0, _Bins = []}}).
records_([], R) ->
    R;
records_([{Tag, Fs} | RTail], {Rcnt, OffF, Rs, FsR}) when is_atom(Tag) ->
    Fcnt = length(Fs),
    records_(RTail, {Rcnt+1, OffF + Fcnt, [{Tag,  OffF, Fcnt} | Rs] , fields1(Fs, FsR)}).
fields1([], R) ->
    R;
fields1( [Name|NTail], {Fields, OffB, Bins}  ) when is_atom(Name) ->
    Bin = iolist_to_binary(["\"", atom_to_binary(Name, latin1),<<"\": ">>]),
    LenB = size(Bin),
    fields(NTail, {[{OffB, LenB} | Fields], OffB + LenB, [Bin|Bins]}).
fields([], R) ->
    R;
fields( [Name|NTail], {Fields, OffB, Bins}  ) when is_atom(Name) ->
    Bin = iolist_to_binary([",\"", atom_to_binary(Name, latin1),<<"\": ">>]),
    LenB = size(Bin),
    fields(NTail, {[{OffB, LenB} | Fields], OffB + LenB, [Bin|Bins]}).

%%@doc Decode JSON to Erlang term.
-spec decode(JSON) -> JSON_TERM when
      JSON      :: binary(),
      JSON_TERM :: any().
decode(_) ->
    not_loaded(?LINE).

%%@doc Decode JSON to Erlang term with options.
-spec decode(JSON, Options) -> JSON_TERM when
      JSON      :: binary(),
      Options   :: [{format, struct|eep18|proplist}],
      JSON_TERM :: any().
decode(B, []) -> decode2(B, struct);
decode(B, [{format, struct}]) ->   decode2(B, struct);
decode(B, [{format, eep18}]) ->    decode2(B, eep18);
decode(B, [{format, proplist}]) -> decode2(B, proplist);
decode(B, [_|XS]) -> decode(B, XS).

decode2(_JSON, _Options) ->
    not_loaded(?LINE).

%% Init

init() ->
    So = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(So, [[json, struct, proplist, eep18, no_match], [true, false, null],
			 [error, big_num, invalid_string, invalid_json, trailing_data]]).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).


%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test encode numeric
enc1_test() ->   <<"1">> = jsonx:encode(1).
enc2_test() ->   <<"-1">> = jsonx:encode(-1).
enc3_test() ->   <<"123456789012345">> = jsonx:encode(123456789012345).
enc4_test() ->   <<"-123456789012345">> = jsonx:encode(-123456789012345).
enc5_test() ->   <<"-1">> = jsonx:encode(-1.0).
enc6_test() ->   <<"-1.1">> = jsonx:encode(-1.1).
enc7_test() ->   <<"11.11">> = jsonx:encode(11.11).
enc8_test() ->   <<"11">> = jsonx:encode(11.00).
enc9_test() ->   <<"65">> = jsonx:encode($A).

%% Test encode atom
enca0_test() -> <<"\"\"">> = jsonx:encode('').
enca1_test() -> <<"\"atom\"">> = jsonx:encode(atom).
enca2_test() -> <<"[true,null,false,\"atom\"]">> = jsonx:encode([true, null, false, atom]).
enca3_test() -> <<"{\"null\":null,\"atom\":\"atom\"}">> = jsonx:encode([{null,null},{atom,atom}]).

%% Test encode binary
encb0_test() -> <<"\"\"">> = jsonx:encode('').
encb1_test() -> <<"\"binary\"">> = jsonx:encode(binary).
encb2_test() -> <<"\"binary\"">> = jsonx:encode('binary').
    
%% Test encode list
encl0_test() ->  <<"[]">> = jsonx:encode([]).
encl1_test() ->  <<"[[[]]]">> = jsonx:encode([[[]]]).
encl2_test() ->  <<"[true,1,1.1,\"bin\"]">> = jsonx:encode([true, 1, 1.1, <<"bin">>]).

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

%% Test encode {json, iolist}
encjs0_test() -> <<>> = jsonx:encode({json, []}).
encjs1_test() -> <<>> = jsonx:encode({json, <<"">>}).
encjs2_test() -> <<"[22,23,24]">> = jsonx:encode({json, <<"[22,23,24]">>}).

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

%% Test encode records
encrec0_test() ->
    F = jsonx:encoder([ {none, []}, {person, [name, age]}, {person2, [name, age, phone]} ]),
    <<"[{},{\"name\": \"IvanDurak\",\"age\": 16},{\"name\": \"BabaYaga\",\"age\": 116,\"phone\": 6666666}]">>
	=  F([ {none}, {person,<<"IvanDurak">>,16}, {person2, <<"BabaYaga">>, 116, 6666666} ]).
-record(none, {}).
-record(person, {name :: binary(), age :: number()}).
-record(person2, {name, age, phone}).
encrec1_test() ->
    F = jsonx:encoder([ {none, record_info(fields, none)}, {person,  record_info(fields, person)}, {person2,  record_info(fields, person2)} ]),
    <<"[{},{\"name\": \"IvanDurak\",\"age\": 16},{\"name\": \"BabaYaga\",\"age\": 116,\"phone\": 6666666}]">>
	=  F([ {none}, {person,<<"IvanDurak">>,16}, {person2, <<"BabaYaga">>, 116, 6666666} ]).

%% Test decode atoms
dectrue_test() ->
    true = jsonx:decode(<<"true">>).
decfalse_test() ->
    false = jsonx:decode(<<"false">>).
decnull_test() ->
    null = jsonx:decode(<<"null">>).

%% Test decode numerics
decnum0_test() ->
    0 = jsonx:decode(<<"0">>).
decnumb_test() ->
    1234567890123456789 = jsonx:decode(<<"1234567890123456789">>).
decnumm_test() ->
    -123 = jsonx:decode(<<"-123">>).
decnum00_test() ->
    0.0 = jsonx:decode(<<"0.0">>).
decnum1_test() ->
    -0.0012 = jsonx:decode(<<"-1.2e-3">>).

%% Test decode string
decstr0_test() ->
    <<>> = jsonx:decode(<<"\"\"">>).
decstr1_test() ->
    <<"...">> = jsonx:decode(<<"\"...\"">>).
decstr2_test() ->
    <<192, 128, 224, 128, 128, 240, 128, 128, 128>> = jsonx:decode(<<34,192,128,224,128,128,240,128,128,128,34>>).
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

%% Test decode array
decarr0_test() ->
    [] = jsonx:decode(<<"[]">>).
decarr1_test() ->
    [[],[[]],[1],[true,false]] = jsonx:decode(<<" [[], [[]], [1], [true, false]] ">>).

%% Test decode object
decobj0_test() ->
    {struct,[]} = jsonx:decode(<<"{}">>).
decobj1_test() ->
    {struct,[{<<"a">>,1}]} = jsonx:decode(<<"{\"a\": 1}">>).
decobj2_test() ->
    {struct,[{<<"a">>,1}, {<<"XX">>, [true, false]}]} = jsonx:decode(<<"{\"a\": 1, \"XX\": [true, false]}">>).

%% Test decode/2
dec2obj0_test() ->
    {struct,[]} = jsonx:decode(<<"{}">>, []).
dec2obj1_test() ->
    {struct,[]} = jsonx:decode(<<"{}">>, [{format, struct}]).
dec2obj2_test() ->
    {[]} = jsonx:decode(<<"{}">>, [{format, eep18}]).
dec2obj3_test() ->
    [] = jsonx:decode(<<"{}">>, [{format, proplist}]).
dec2obj4_test() ->
    [] = jsonx:decode(<<"{}">>, [{format, eep18}, {format, struct}, {format, proplist}]).

%% Test trailing data
dectre_test() ->
 {error,trailing_data,4} = jsonx:decode(<<"123 ,">>).

%% Test stack
encdec0_test() ->
    L = lists:seq(0,99),
    LL = L ++ L,
    LL = jsonx:decode(jsonx:encode(LL)).

-endif.
