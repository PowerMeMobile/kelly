-module(k_http_api_handler_downlink_stats).

-behaviour(gen_cowboy_crud).

%% gen_cowboy_crud callbacks
-export([
	init/0,
	create/1,
	read/1,
	update/1,
	delete/1
]).

-include("crud_specs.hrl").
-include_lib("k_common/include/logging.hrl").

%% ===================================================================
%% gen_cowboy_crud callbacks
%% ===================================================================

init() ->
	Read = #method_spec{
				path = [<<"report">>, <<"downlink">>],
				params = []},

	{ok, #specs{
		create = undefined,
		read = Read,
		update = undefined,
		delete = undefined
	}}.

read(Params) ->
	case k_statistic:downlink_report() of
		{ok, Report} ->
			{ok, Report};
		{error, Error} ->
			?log_debug("Downlink report failed with: ~p", [Error]),
			{exception, 'svc0003'}
	end.

create(_Params) ->
	ok.

update(_Params) ->
	ok.

delete(_Params) ->
	ok.