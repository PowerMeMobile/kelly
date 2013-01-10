-module(kelly_http_api_gtw_test).

%% -compile(export_all).

-include_lib ("etest_http/include/etest_http.hrl").
-include_lib("eunit/include/eunit.hrl").

gtw_test_() ->
	{"Gateway & its connections http interface tests",
	{setup,
		fun delete_gtw/0,
		{inorder, [?_test(create_gtw()),
					?_test(update_gtw()),
					?_test(create_connection()),
					?_test(update_connection()),
					?_test(delete_connection()),
					?_test(delete_gtw())]
		}
	}}.

%% ===================================================================
%% GATEWAY Tests
%% ===================================================================

gtw_id() ->
	<<"7dc235d0-c938-4b66-8f8c-c9037c7eace7">>.

gtw_path() ->
	"http://127.0.0.1:8080/gateways".
gtw_path(ID) ->
	gtw_path() ++ "/" ++ binary_to_list(ID).

create_gtw() ->
	Url = gtw_path(),
	GtwID = gtw_id(),
	GtwName = <<"test_gtw">>,
	GtwRPS = 10000,
	Queries = [
		{id, GtwID},
		{name, GtwName},
		{rps, GtwRPS}],
    Response = ?perform_post(Url, [], <<>>, Queries),
    ?assert_status(201, Response),
	?assert_json_values(Queries, Response).

update_gtw() ->
	GtwID = gtw_id(),
	Url = gtw_path(GtwID),
	%% is exist
	Response = ?perform_get(Url),
	?assert_status(200, Response),
	?assert_json_value(id, GtwID, Response),

	%% update name
	NewName = <<"new_name">>,
	UpdateNameQueries = [{name, NewName}],
	PutResp = ?perform_put(Url, [], <<>>, UpdateNameQueries),
	?assert_status(200, PutResp),
	?assert_json_value(name, NewName, PutResp),

	%% update rps
	NewRPS = 12345,
	UpdRPSQueries = [{rps, NewRPS}],
	UpdateRPSResp = ?perform_put(Url, [], <<>>, UpdRPSQueries),
	?assert_status(200, UpdateRPSResp),
	?assert_json_value(rps, NewRPS, UpdateRPSResp).

delete_gtw() ->
	delete_gtw(gtw_id()).

delete_gtw(GtwID) ->
	Url = gtw_path(GtwID),
	delete_req(Url).

%% ===================================================================
%% Gtw CONNECTION Tests
%% ===================================================================

conn_id() ->
	0.

conn_path(GtwID, ConnID) ->
	conn_path(GtwID) ++ "/" ++ integer_to_list(ConnID).
conn_path(GtwID) ->
	gtw_path(GtwID) ++ "/connections".

create_connection() ->
	Url = conn_path(gtw_id()),
	ConnID = conn_id(),
	Queries = [{id, ConnID},
				{type, 1},
				{addr, <<"127.0.0.1">>},
				{port, 8001},
				{sys_id, <<"smppclient1">>},
				{pass, <<"password">>},
				{sys_type, <<"smppclient1">>},
				{addr_ton, 1},
				{addr_npi, 0},
				{addr_range, <<"">>}],
	Resp = ?perform_post(Url, [], <<>>, Queries),
	?assert_status(201, Resp),
	?assert_json_values(Queries, Resp).

update_connection() ->
	ConnID = conn_id(),
	Url = conn_path(gtw_id(), ConnID),
	Queries = [{type, 2},
				{addr, <<"127.0.0.2">>},
				{port, 8002},
				{sys_id, <<"smppclient2">>},
				{pass, <<"password2">>},
				{sys_type, <<"smppclient2">>},
				{addr_ton, 2},
				{addr_npi, 2},
				{addr_range, <<"">>}],
	Resp = ?perform_put(Url, [], <<>>, Queries),
	?assert_status(200, Resp),
	?assert_json_values(Queries, Resp).

delete_connection() ->
	delete_connection(conn_id()).
delete_connection(ConnID) ->
	Url = conn_path(gtw_id(), ConnID),
	delete_req(Url).

%% ===================================================================
%% Internals
%% ===================================================================

delete_req(Url) ->
    Response = ?perform_delete(Url),
    ?assert_status(204, Response).
