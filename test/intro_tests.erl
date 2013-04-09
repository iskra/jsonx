-module(intro_tests).
-include_lib("eunit/include/eunit.hrl").


%% Test trailing data
trail_test() ->
 {error,trailing_data,4} = jsonx:decode(<<"123 ,">>).

%% Test stack
stack0_test() ->
    L = lists:seq(0,99),
    LL = L ++ L,
    LL = jsonx:decode(jsonx:encode(LL)).

stack1_test() ->
    L = {[{<<"a">>, lists:seq(0,99)}|| _X <- lists:seq(0,99)]},
    L = jsonx:decode(jsonx:encode(L)).

stack2_test() ->
    D = jsonx:decoder([ {r0, [q,w,e,r,t,y,u,i,o,p, a,s,d,f,g,h,j,k,l,z ]} ]),
    E = jsonx:encoder([ {r0, [q,w,e,r,t,y,u,i,o,p, a,s,d,f,g,h,j,k,l,z ]} ]),
    JTerm = list_to_tuple([r0|lists:seq(1,20)]),
    Json = E(JTerm),
    JTerm = D(Json ).

