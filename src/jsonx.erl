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
%%       <li>object -> {PropList}, optional struct or proplist.</li>
%%       <li>object -> #record{...} - decoder must be predefined</li>
%%      </ul>
%%      <h3>Encode (erlang -> json)</h3>
%%      <ul>
%%       <li>atom null 	      -> null</li>
%%       <li>atom true 	      -> true</li>
%%       <li>atom true 	      -> false</li>
%%       <li>any other atom     -> string</li>
%%       <li>binary             -> string</li>
%%       <li>number             -> number</li>
%%       <li>map                -> object</li>
%%       <li>{struct, PropList} -> object</li>
%%       <li>{PropList}         -> object</li>
%%       <li>PropList           -> object</li>
%%       <li>#record{...}       -> object - encoder must be predefined</li>
%%       <li>{json, IOList}     -> include IOList with no validation</li>
%%      </ul>

-module(jsonx).
-export([encode/1, decode/1, decode/2,
         encoder/1, encoder/2, decoder/1, decoder/2]).
-on_load(init/0).
-define(LIBNAME, jsonx).
-define(APPNAME, jsonx).

%% =================
%% API Encoding JSON
%% =================

%%@doc Encode JSON.
-spec encode(JSON_TERM) -> JSON when
      JSON      :: binary(),
      JSON_TERM :: any().
encode(JSON_TERM)->
    encode1(JSON_TERM).

%%@doc Build a JSON encoder.
-spec encoder(RECORDS_DESC) -> ENCODER when
      RECORDS_DESC :: [{tag, [names]}],
      ENCODER      :: function().
encoder(Records_desc) ->
    encoder(Records_desc, []).

%%@doc Build a JSON encoder.
-spec encoder(RECORDS_DESC, OPTIONS) -> ENCODER when
      RECORDS_DESC :: [{tag, [names]}],
      OPTIONS      :: [{ignore, [atom()]}],
      ENCODER      :: function().
encoder(Records_desc, Options) ->
    {Rcnt, Fcnt, Binsz, Records, Fields, Bin} = prepare_enc_desc(Records_desc),
    Ignored = proplists:get_value(ignore, Options, []),
    Resource = make_encoder_resource(Rcnt, Fcnt, Records, Fields, Binsz, Bin, Ignored),
    fun(JSON_TERM) -> encode_res(JSON_TERM, Resource) end.

%% ==================
%% API Decoding JSON
%% ==================

%%@doc Decode JSON to Erlang term.
-spec decode(JSON) -> JSON_TERM when
      JSON      :: binary(),
      JSON_TERM :: any().
decode(JSON) ->
    decode_opt(JSON, eep18).

%%@doc Decode JSON to Erlang term with options.
-spec decode(JSON, OPTIONS) -> JSON_TERM when
      JSON      :: binary(),
      OPTIONS   :: [{format, struct|eep18|map|proplist}],
      JSON_TERM :: any().
decode(JSON, Options) ->
    case parse_format(Options) of
	undefined -> decode_opt(JSON, eep18);
	F         -> decode_opt(JSON, F)
    end.

%%@doc Build a JSON decoder.
-spec decoder(RECORDS_DESC) -> DECODER when
      RECORDS_DESC :: [{tag, [names]}],
      DECODER      :: function().
decoder(Records_desc) ->
    {RecCnt, UKeyCnt, KeyCnt, UKeys, Keys, Records3} = prepare_for_dec(Records_desc),
    Resource = make_decoder_resource(RecCnt, UKeyCnt, KeyCnt, UKeys, Keys, Records3),
     fun(JSON_TERM) -> decode_res(JSON_TERM, eep18, Resource, true) end.
	    
%%@doc Build a JSON decoder with output undefined objects.
-spec decoder(RECORDS_DESC, OPTIONS) -> DECODER when
      RECORDS_DESC :: [{tag, [names]}],
      OPTIONS      :: [{format, struct|eep18|map|proplist}],
      DECODER      :: function().
decoder(Records_desc, Options) ->
    {RecCnt, UKeyCnt, KeyCnt, UKeys, Keys, Records3} = prepare_for_dec(Records_desc),
    Resource = make_decoder_resource(RecCnt, UKeyCnt, KeyCnt, UKeys, Keys, Records3),
    %%Format = parse_format(Options),
    case parse_format(Options) of
	undefined -> fun(JSON_TERM) -> decode_res(JSON_TERM, eep18, Resource, false) end;
	Format    -> fun(JSON_TERM) -> decode_res(JSON_TERM, Format, Resource, false) end
    end.

%% ==========
%% Call NIFs
%% ==========

encode1(_JSON_TERM) ->
    not_loaded(?LINE).

encode_res(_JSON_TERM, _RESOURCE) ->
    not_loaded(?LINE).

decode_opt(_JSON, _FORMAT) ->
    not_loaded(?LINE).

decode_res(_JSON_TERM, _FORMAT, _RESOURCE, _STRICT_FLAG) ->
    not_loaded(?LINE).

make_encoder_resource(_Rcnt, _Fcnt, _Records, _Fields, _Binsz, _Bin, _Ignored) ->
    not_loaded(?LINE).

make_decoder_resource(_RecCnt, _UKeyCnt, _KeyCnt, _UKeys, _Keys, _Records3) ->
    not_loaded(?LINE).

%% =================
%% Private functions
%% =================

parse_format([]) ->
    undefined;
parse_format([{format, struct} | _]) ->
    struct;
parse_format([{format, proplist} | _]) ->
    proplist;
parse_format([{format, eep18} | _]) ->
    eep18;
parse_format([{format, map} | _]) ->
	maybe_map();
parse_format([_H | T]) ->
    parse_format(T).


-ifndef(JSONX_NO_MAPS).
maybe_map() -> map.
-else.
maybe_map() -> undefined.
-endif.

%%%% Internal for decoder

prepare_for_dec(Records) ->
    RecCnt = length(Records),
    Records1 = lists:ukeysort(1,Records),
    RecCnt1 = length(Records1),
    case (RecCnt1 == RecCnt) of
	true ->
	    UKeys = lists:usort(lists:flatten([Ks || {_Tag, Ks} <- Records1])),
	    {UKeyCnt, EnumUKeys} = enumerate(UKeys),
	    Records2 = [{Tag, length(Keys), [ findpos(EnumUKeys, K) || K <- Keys] } || {Tag, Keys} <- Records1],
	    {KeyCnt, Records3, Keys} = scan_records(Records2),
	    { RecCnt         %% Records Counter
	      , UKeyCnt      %% Uniq Keys Counter
	      , KeyCnt       %% Keys Counter
	      , UKeys        %% [Key]
	      , Keys         %% [KeyNum] 
	      , Records3     %% [{Tag, Off, Len}]
	    };
	false ->
	    {error, invalid_input}
    end.

scan_records(Records2) ->
    scan_records({0, [], []}, Records2).
scan_records({Offs, AccR, AccK}, []) ->
    {Offs, lists:reverse(AccR),  lists:reverse(AccK)};
scan_records({Offs, AccR, AccK}, [{Tag, Len, KeyNums} | Ts]) ->
    scan_records({Offs + Len, [{Tag, Offs, Len} | AccR], lists:reverse(KeyNums, AccK)}, Ts).

findpos(EnumKeys, Key) ->
    {Num, _Key} = lists:keyfind(Key, 2, EnumKeys),
    Num.

enumerate(Xs) ->
    enumerate(0, [], Xs ).
enumerate(N, Acc, []) ->
    {N, lists:reverse(Acc)};
enumerate(N, Acc, [H| Ts]) ->
    enumerate(N + 1, [{N, H} | Acc], Ts).

%%%% Internal for encoder
 
prepare_enc_desc(T) ->
    {Rcnt, Fcnt, Records, {Fields, Blen, Bins}} = prepare_enc_desc1(T),
    {Rcnt, Fcnt, Blen, lists:reverse(Records), lists:reverse(Fields),
     iolist_to_binary(lists:reverse(Bins))}.
prepare_enc_desc1(Records) ->
    prepare_enc_desc2(Records, {_Rcnt = 0, _OffF = 0, _Records = [],
	    {_Fields = [], _OffB = 0, _Bins = []}}).
prepare_enc_desc2([], R) ->
    R;
prepare_enc_desc2([{Tag, Fields} | RTail], {Rcnt, OffF, Records, FieldsR}) when is_atom(Tag) ->
    Fcnt = length(Fields),
    prepare_enc_desc2(RTail, {Rcnt+1, OffF + Fcnt, [{Tag,  OffF, Fcnt} | Records] , prepare_enc_fields1(Fields, FieldsR)}).
prepare_enc_fields1([], R) ->
    R;
prepare_enc_fields1( [Name|NTail], {Fields, OffB, Bins}  ) when is_atom(Name) ->
    Bin = iolist_to_binary(["\"", atom_to_binary(Name, latin1),<<"\": ">>]),
    LenB = size(Bin),
    prepare_enc_fields(NTail, {[{OffB, LenB} | Fields], OffB + LenB, [Bin|Bins]}).
prepare_enc_fields([], R) ->
    R;
prepare_enc_fields( [Name|NTail], {Fields, OffB, Bins}  ) when is_atom(Name) ->
    Bin = iolist_to_binary([",\"", atom_to_binary(Name, latin1),<<"\": ">>]),
    LenB = size(Bin),
    prepare_enc_fields(NTail, {[{OffB, LenB} | Fields], OffB + LenB, [Bin|Bins]}).

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
	ok = erlang:load_nif(So, [[json, struct, proplist, eep18, map, no_match], [true, false, null],
			 [error, big_num, invalid_string, invalid_json, trailing_data, undefined_record]]).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
