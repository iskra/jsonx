%% @copyright 2013 Yuriy Iskra <iskra.yw@gmail.com>

%% @doc Write Me
%%

-module(jstream).
-export([new_decoder/1, update_decoder/2, get_event/1]).
-on_load(init/0).
-define(LIBNAME, jstream).
-define(APPNAME, jsonx).

%% ==================
%% API Decoding doc
%% ==================

%%@doc Make new decocoder.
-spec new_decoder(JSON) -> DECODER when
      JSON  :: binary(),
      DECODER :: any().
new_decoder(_JSON) ->
    not_loaded(?LINE).

%%@doc Add new data to decoder.
-spec update_decoder(DECODER, JSON) -> ok when
      JSON  :: binary(),
      DECODER :: any().
update_decoder(_DECODER, _JSON) ->
    not_loaded(?LINE).

%%@doc Get next event.
-spec get_event(DECODER) -> EVENT when
      DECODER  :: any(),
      EVENT :: [true | false | null | binary() | number() |
		start_map| {map_key, binary()} | end_map |
		start_array | end_array |
		{error, any()} |  parse_buf | parse_end].
get_event(_DECODER) ->
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
    erlang:load_nif(So, [null, start_map, map_key, end_map, start_array, end_array, parse_buf, parse_end,
			 big_num, invalid_string, invalid_json]).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).


