-module(bench_encode_records).
-export([test/0]).
-record(person, {name, last_name, age, adress, phones, email}).

data() ->
    #person{name = <<"Baba">>,
	    last_name = <<"Yaga">>,
	    age = 116,
	    adress = <<"ИзбаНаОкорочках, Болото, Лес, Россия">>,
	    phones = [<<"666-66-66">>],
	    email = baba_yaga@les.ru
	   }.

test() ->
    Format = "~7s~12s~12s~12s~12s~12s~n",
    io:format(Format, ["Record", "Jsonx",    "Jsonx", "Jiffy",    "Jiffy", "jsonx"]),
    io:format(Format, ["Count",  "Out Size", "Time",  "Out Size", "Time",  "vs jiffy"]),
    [test(X) || X <- [1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683]].

test(Times) ->
    FJsonx = jsonx:encoder([ {person, record_info(fields, person)} ]),

    Names = record_info(fields, person),
    FJiffy =  fun(Rs) ->
		      jiffy:encode( [{lists:zip(Names, tuple_tail(R))} || R <- Rs] )
	      end,
    Data =  [data() || _X <- lists:seq(1, Times)],
    _A = timer:tc(FJiffy, [Data]),
    _B = timer:tc(FJsonx, [Data]),
    {Jf_tc, Jf_bin} = timer:tc(FJiffy, [Data]),
    {Jx_tc, Jx_bin} = timer:tc(FJsonx, [Data]),
    Out = [Times, size(Jx_bin), Jx_tc, size(Jf_bin), Jf_tc, Jf_tc/Jx_tc],    
    io:format("~7b~11bb~12b~11bb~12b~12.2f~n", Out).

%%   .
tuple_tail(Rec) ->
    [_H|RT] = tuple_to_list(Rec),
    RT.
