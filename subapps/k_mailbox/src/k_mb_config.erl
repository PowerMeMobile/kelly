-module(k_mb_config).

-include("application.hrl").

%% ===================================================================
%% API Functions Exports
%% ===================================================================

-export([
	init/0,
	get_env/1
]).

%% ===================================================================
%% API Function Definitions
%% ===================================================================

-spec init() -> ignore.
init() ->
	% data type verification
	true = is_integer(get_env(request_timeout)),

	% convert expiration date from hours to secs
	ExpirationDate = convert_to_sec(get_env(expiration_date)),
	application:set_env(?APP, expiration_date, ExpirationDate, 5000),

	% convert purge rate form hours to msecs
	PurgeRate = convert_to_msec(get_env(purge_rate)),
	application:set_env(?APP, purge_rate, PurgeRate, 5000).

-spec get_env(Key :: term()) -> Value :: term().
get_env(Key) ->
	case application:get_env(?APP, Key) of
		undefined -> default(Key);
		{ok, Val} -> Val
	end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

default(pool_size)					-> 10;

default(request_timeout) 			-> 10000;
default(repeat_delay)	 			-> 5000;
default(max_retry)		   			-> 5;

default(expiration_date) 	 		-> 48; % hours
default(purge_rate)					-> 2; % hours

default(reply_to) 					-> <<"pmm.kelly.mailbox_reply">>;

default(_Key)            			-> undefined.


convert_to_sec(Hours) ->
	Hours * 3600.

convert_to_msec(Hours) ->
	Hours * 216000.
