-module(k_common_sup).

-behaviour(supervisor).

-export([
	start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

-include("logging.hrl").
-include("supervisor_spec.hrl").
-include("storages.hrl").

-define(MongoStorageSpec(Name),
	{Name, {mongodb_storage, start_link, [Name]}, permanent, 1000000, worker, [mongodb_storage]}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	?log_debug("init", []),
    {ok, {
		{one_for_one, 5, 10}, [
			{k_mnesia_schema, {k_mnesia_schema, start_link, []}, permanent, 100000, worker, [k_mnesia_schema]},
			?MongoStorageSpec(?networkStorageName),
			?MongoStorageSpec(?providerStorageName),
			?MongoStorageSpec(?gatewayStorageName),
			?MongoStorageSpec(?customerStorageName),
			?MongoStorageSpec(mt_messages),
			?MongoStorageSpec(mo_messages),
			?MongoStorageSpec(k1api_sms_request_id_to_msg_ids)
		]}
	}.
