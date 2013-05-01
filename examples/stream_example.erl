-module(stream_example).
-compile(export_all).

run() ->
    run("example.json").

run(FileName) ->
    {ok,FD} = file:open(FileName, read),
    Dec = jstream:new_decoder(<<>>),
    R = all_events_from_lines(FD, Dec, {[], <<"">>}),
    file:close(FD), R.


all_events_from_lines(FD, Dec, {Path, CurKey} = State) ->
    D1 = jstream:get_event(Dec),
    %%io:format("~w~n", [D1]),
    case D1 of
	parse_buf ->
	    %%lists:reverse([D1 | Acc]);
	    {ok, Ln} = file:read_line(FD),
	    ok = jstream:update_decoder(Dec, list_to_binary(Ln)),
	    all_events_from_lines(FD, Dec, State);
	{error, _E } ->
	    D1;
	{parse_end, _B} ->
	    ok;
	start_map ->
	    all_events_from_lines(FD, Dec, {[CurKey|Path], CurKey});
	end_map ->
	    [_ | P] = Path,
	    all_events_from_lines(FD, Dec, {P, CurKey});  
	{map_key, K} ->
	    all_events_from_lines(FD, Dec, {Path, K});
	X ->
	    io:format("~s = ~p~n", [ join_r([CurKey|Path], <<"/">>), X ]),
	    all_events_from_lines(FD, Dec, State)
    end.

join_r(List, Sep) ->
    join_r(List, Sep, []).

join_r([], _Sep, Acc) ->
   Acc;
join_r([T], _Sep, Acc) ->
    [T|Acc];
join_r([H|Ts], Sep, Acc) ->
    join_r(Ts, Sep, [Sep, H] ++ Acc).
