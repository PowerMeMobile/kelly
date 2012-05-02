-type snmp_index() :: list().
-type snmp_column_name() :: atom().
-type snmp_value() :: term().
-type er_snmp_state() :: term().
-type snmp_error_reason() :: term().
-type snmp_value_list() :: [{snmp_column_name(), snmp_value()}].
-type snmp_result() :: {ok, snmp_value()} |
						{error, noSuchObject} |
						{error, noSuchInstance} |
						{error, er_snmp_state()} |
						{error, snmp_error_reason()}.

-record(task,{
	id :: integer(),
	function :: atom(),
	args :: tuple()
}).