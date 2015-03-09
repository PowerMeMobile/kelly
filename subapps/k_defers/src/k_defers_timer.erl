-module(k_defers_timer).

-behaviour(gen_server).

-ignore_xref([{start_link, 0}]).

%% API
-export([
    start_link/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").

-record(st, {}).

-define(TIMEOUT, (1000 * 60 * 1)).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #st{}, ?TIMEOUT}.

handle_call(_Request, _From, St) ->
    {stop, unexpected_call, St}.

handle_cast(Req, St) ->
    {stop, {unexpected_cast, Req}, St}.

handle_info(timeout, St) ->
    Ts = ac_datetime:utc_timestamp(),
    case k_storage_defers:get_expired_up_to(Ts) of
        {ok, DefReqs} ->
            ok = publish_def_reqs(DefReqs);
        {error, Error} ->
            ?log_error("Get expired defers failed with: ~p", [Error]),
            ignore
    end,
    {noreply, St, ?TIMEOUT};
handle_info(_Info, St) ->
    {stop, unexpected_info, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% ===================================================================
%% Internal
%% ===================================================================

publish_def_reqs([]) ->
    ok;
publish_def_reqs([{ReqId, Reqs} | DefReqs]) ->
    publish_req(ReqId, Reqs),
    publish_def_reqs(DefReqs).

publish_req(ReqId, []) ->
    ok = k_storage_defers:delete(ReqId);
publish_req(ReqId, [{GtwId, Payload} | Reqs]) ->
    ok = k_defers_publisher:publish(ReqId, GtwId, Payload),
    ok = k_storage_defers:delete(ReqId, GtwId),
    publish_req(ReqId, Reqs).
