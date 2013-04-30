-module(jstream_test).
-include_lib("eunit/include/eunit.hrl").
-export([all_events/1, all_events_from_blist/1]).

all_events(J) when is_binary(J) ->
    D = jstream:new_decoder(J),
    all_events(D, []).

all_events(D, Acc) ->
    case D1 = jstream:get_event(D) of
	parse_buf ->
	    lists:reverse([D1 | Acc]);
	{error, _ } ->
	    lists:reverse([D1 | Acc]);
	{parse_end, _B} ->
	    lists:reverse([D1 | Acc]);
	X ->
	    all_events(D, [X|Acc])
    end.

all_events_from_blist([J|Js]) ->
    D = jstream:new_decoder(J),
    all_events_from_blist(D, Js, []).

all_events_from_blist(D, Js, Acc) ->
    case D1 = jstream:get_event(D) of
	parse_buf ->
	    %%lists:reverse([D1 | Acc]);
	    [J| JTs] = Js,
	    ok = jstream:update_decoder(D, J),
	    all_events_from_blist(D, JTs, [D1|Acc]);
	{error, _ } ->
	    lists:reverse([D1 | Acc]);
	{parse_end, _B} ->
	    lists:reverse([D1 | Acc]);
	_ ->
	    all_events_from_blist(D, Js, [D1|Acc])
    end.

scalar_test() ->
    [1,{parse_end,<<>>}] =  jstream_test:all_events(<<" 1 ">>),
    [1.2,{parse_end,<<>>}] =  jstream_test:all_events(<<" 1.2 ">>),
    [true,{parse_end,<<>>}] =  jstream_test:all_events(<<" true ">>),
    [false,{parse_end,<<>>}] =  jstream_test:all_events(<<" false ">>),
    [null,{parse_end,<<>>}] =  jstream_test:all_events(<<" null ">>),
    [<<"String">>,{parse_end,<<>>}] =  jstream_test:all_events(<<" \"String\" ">>),
    [<<"String">>,{parse_end,<<"rest">>}] =  jstream_test:all_events(<<" \"String\" rest">>).

array_test() ->
    [start_array,end_array,{parse_end,<<>>}] = jstream_test:all_events(<<" [] ">>),
    [{error,invalid_json}] = jstream_test:all_events(<<"] ">>),
    [start_array,parse_buf] = jstream_test:all_events(<<"[ ">>),
    [start_array,{error,invalid_json}] = jstream_test:all_events(<<"[, ">>),
    [start_array,start_array,end_array,{error,invalid_json}] = jstream_test:all_events(<<"[[],]">>),
    [start_array,1,end_array,{parse_end,<<>>}] = jstream_test:all_events(<<"[1] ">>),
    [start_array,1,2,end_array,{parse_end,<<>>}] = jstream_test:all_events(<<"[1,2] ">>),
    [start_array,1,2,{error,invalid_json}] = jstream_test:all_events(<<"[1,2x] ">>),
    [start_array,1,2,{error,invalid_json}] = jstream_test:all_events(<<"[1,2,] ">>),
    [start_array,start_array,end_array,end_array,{parse_end,<<>>}] = jstream_test:all_events(<<"[[]]">>),
    [start_array,start_array,1,end_array,end_array, {parse_end,<<>>}] = jstream_test:all_events(<<"[[1]] ">>),
    [start_array,start_array,1,end_array,end_array, {parse_end,<<"rest">>}] = jstream_test:all_events(<<"[[1]] rest">>),
    [start_array,start_array,1,end_array,parse_buf] = jstream_test:all_events(<<" [[1] ">>),
    [start_array,1,start_array,2,start_array,end_array,end_array,start_array,start_array,end_array,4,end_array,
     end_array,{parse_end,<<>>}] = jstream_test:all_events(<<"[1,[2,[]], [[],4]]">>).

object_test() ->
    [start_map,end_map,{parse_end,<<>>}] = jstream_test:all_events(<<"{}">>),
    [start_map,{map_key,<<"k">>},1,end_map,{parse_end,<<>>}] = jstream_test:all_events(<<"{\"k\": 1}">>),
    [start_map,{map_key,<<"k1">>},1,{map_key,<<"k2">>},2,end_map,{parse_end,<<>>}] =
	jstream_test:all_events(<<"{\"k1\": 1, \"k2\": 2}">>),
    [start_map,{map_key,<<"k1">>},1,{map_key,<<"k2">>},2,{map_key,<<"k3">>},3,end_map,{parse_end,<<>>}] =
	jstream_test:all_events(<<"{\"k1\" :1, \"k2\": 2, \"k3\" : 3}">>),
    [start_map,{map_key,<<"k1">>},1,{map_key,<<"k2">>},2,{error,invalid_json}] = jstream_test:all_events(<<"{\"k1\": 1, \"k2\": 2]">>),
    [start_map,{map_key,<<"k1">>},1,{error,invalid_json}] = jstream_test:all_events(<<"{\"k1\": 1 \"k2\": 2]">>).

complex_test() ->
    [start_array,
     start_map,end_map,
     start_map,{map_key,<<"k0">>},true,end_map,
     start_map,{map_key,<<"k1">>},start_array,11,1.1,start_map,{map_key,<<"k3">>},3,end_map,end_array,end_map,
     start_map,{map_key,<<"superkey">>},start_map,{map_key,<<"subkey">>},null,end_map,end_map,
     end_array,{parse_end,<<"...rest">>}] =
	jstream_test:all_events(<<"[{}, {\"k0\": true}, {\"k1\": [11, 1.1, {\"k3\":3}]}, {\"superkey\": {\"subkey\":null}}] ...rest">>). 

error_test() ->
    [{error,invalid_json}] = jstream_test:all_events(<<",">>),
    [{error,big_num}] = jstream_test:all_events(<<"12345678901234567890">>),
    [{error,invalid_string}] = jstream_test:all_events(<<"\"",1,127,"\"">>).

list_chunks_test() ->
    [start_array,1,22,parse_buf,333,parse_buf,444,parse_buf,5111,parse_buf,5,start_array,parse_buf,end_array,end_array,{parse_end,<<>>}]
	=  jstream_test:all_events_from_blist([<<"[1, 22,">>, <<"333">>, <<",444">>, <<",5111,">>, <<"5, [">>,<<"]]">>]),
    [start_array,parse_buf,1234567,parse_buf,parse_buf,
     234567890123456789,parse_buf,start_array,parse_buf,
     end_array,parse_buf,end_array, {parse_end,<<>>}]
	= jstream_test:all_events_from_blist([<<"[">>, <<"1234567">>, <<",">>, <<"234567890123456789,">>, <<"[">>,<<"]">>, <<"]">>]).

obj_chunks_test() ->
    [start_map,parse_buf,{map_key,<<"k1">>},parse_buf,true,parse_buf,{map_key,<<"k2">>},
     parse_buf,false,parse_buf,end_map,{parse_end,<<>>}]
	= jstream_test:all_events_from_blist([<<"{">>,<<"\"k1\":">>, <<"true , ">>, <<"\"k2\":">>,<<"false">>,<<"}">>]),
    [start_map,parse_buf,{map_key,<<"k1">>},parse_buf,true,parse_buf,end_map,{parse_end,<<>>}]
	= jstream_test:all_events_from_blist([<<"{">>,<<"\"k1\":">>, <<"true">>,<<"}">>]),
    [start_map,parse_buf,{map_key,<<"k1">>},parse_buf,true,parse_buf,parse_buf,
     {map_key,<<"k2">>},parse_buf,false,parse_buf,end_map,{parse_end,<<>>}]
	= jstream_test:all_events_from_blist([<<"{">>,<<"\"k1\":">>, <<"true">>, <<",">>, <<"\"k2\":">>,<<"false">>,<<"}">>]),
    [start_map,{map_key,<<"key">>},parse_buf,start_array,1234567,2345,parse_buf,start_map,
     parse_buf,end_map,parse_buf,end_array,end_map,{parse_end,<<>>}]
	= jstream_test:all_events_from_blist([<<"{\"key\":">>, <<"[1234567,2345,">>, <<"{">>,<<"}">>, <<"]}">>]). 

obj_chunks_err_test() ->
    [start_map,parse_buf,{map_key,<<"k1">>},parse_buf,true,parse_buf,{error,invalid_json}]
	=  jstream_test:all_events_from_blist([<<"{">>,<<"\"k1\":">>, <<"true ,">>, <<",\"k2\":">>,<<"false">>,<<"}">>]).
