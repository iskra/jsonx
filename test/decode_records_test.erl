-module(decode_records_test).
-include_lib("eunit/include/eunit.hrl").

-record(r0, {}).
-record(r1, {a}).
-record(r2, {a,b}).
-record(r4, {c,b,q,p}).

decrec0_test() ->
    D = jsonx:decoder([{r0,record_info(fields, r0)}, {r1,record_info(fields, r1)}, {r2, record_info(fields,r2)}, {r4,record_info(fields, r4)}]),
    E = jsonx:encoder([{r0,record_info(fields, r0)}, {r1,record_info(fields, r1)}, {r2, record_info(fields,r2)}, {r4,record_info(fields, r4)}]),
    JTerm = [{r0}, {r1, 11}, {r2, 21, 22}, {r4, 42,41,44,43}, {r2, 211, {r2, 311, 312}}],
    Json  = E(JTerm),
    JTerm = D(Json).

decrec1_test() ->
    D = jsonx:decoder([{r0,record_info(fields, r0)}, {r1,record_info(fields, r1)}, {r2, record_info(fields,r2)}, {r4,record_info(fields, r4)}]),
    E = jsonx:encoder([{r0,record_info(fields, r0)}, {r1,record_info(fields, r1)}, {r2, record_info(fields,r2)}, {r4,record_info(fields, r4)}]),
    JTerm = {r4, {r1, [{r1, {r0}}]}, 413, {r2, 21, {r2, 211, 212}}, 414},
    Json  = E(JTerm),
    JTerm = D(Json).

%% Test nonstrict decoder
decrec2_test() ->
    ND = jsonx:nonstrict_decoder([{r0,record_info(fields, r0)}, {r1,record_info(fields, r1)}, {r2, record_info(fields,r2)}, {r4,record_info(fields, r4)}], [{format, eep18}]),
    E = jsonx:encoder([{r0,record_info(fields, r0)}, {r1,record_info(fields, r1)}, {r2, record_info(fields,r2)}, {r4,record_info(fields, r4)}]),
    JTerm = [{r4, 1, 2, 3, 4}, [{record, undefined}, {strict, false}]],
    Json  = E(JTerm),
    Json = E(ND(Json)).
    
    
