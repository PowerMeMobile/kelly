-module(kelly_http_api_blacklist_test).

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

blacklist_test_() ->
    {"Blacklist http interface tests",
        {setup,
            fun delete_blacklist_entry/0,
            {inorder, [
                ?_test(create_blacklist_entry()),
                ?_test(update_blacklist_1_entry()),
                ?_test(update_blacklist_2_entry()),
                ?_test(delete_blacklist_entry())
            ]}
        }
    }.

%% ===================================================================
%% Blacklist Tests
%% ===================================================================

blacklist_entry_id() ->
    <<"c973e494-c4a4-11e3-8684-00269e42f7a5">>.

blacklist_entry_path() ->
    "http://127.0.0.1:8080/blacklist".

blacklist_entry_path(Id) ->
    blacklist_entry_path() ++ "/" ++ binary_to_list(Id).

create_blacklist_entry() ->
    Url = blacklist_entry_path(),
    EntryId = blacklist_entry_id(),
    DstAddr = {dst_addr, <<"375296660002,1,1">>},
    SrcAddr = {src_addr, <<"Hola,5,0">>},
    Query = [
        {id, EntryId}
    ],
    Resp = ?perform_post(Url, [], <<>>, [DstAddr, SrcAddr | Query]),
    ?assert_status(201, Resp),
    ?assert_json_values(Query, Resp).

update_blacklist_1_entry() ->
    EntryId = blacklist_entry_id(),
    Url = blacklist_entry_path(EntryId),
    DstAddr = {dst_addr, <<"375296660003,1,1">>},
    SrcAddr = {src_addr, <<"">>},
    Query = [
        {id, EntryId}
    ],
    Resp = ?perform_put(Url, [], <<>>, [DstAddr, SrcAddr | Query]),
    ?assert_status(200, Resp),
    ?assert_json_values(Query, Resp).

update_blacklist_2_entry() ->
    EntryId = blacklist_entry_id(),
    Url = blacklist_entry_path(EntryId),
    DstAddr = {dst_addr, <<"375296660003,1,1">>},
    Query = [],
    Resp = ?perform_put(Url, [], <<>>, [DstAddr | Query]),
    ?assert_status(200, Resp),
    ?assert_json_values(Query, Resp).

delete_blacklist_entry() ->
    delete_blacklist_entry(blacklist_entry_id()).

delete_blacklist_entry(Id) ->
    Url = blacklist_entry_path(Id),
    delete_req(Url).

%% ===================================================================
%% Internals
%% ===================================================================

delete_req(Url) ->
    Resp = ?perform_delete(Url),
    ?assert_status(204, Resp).
