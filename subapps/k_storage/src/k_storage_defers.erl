-module(k_storage_defers).

-include("customer.hrl").
-include("msg_info.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_dto/include/adto.hrl").

%% API
-export([
    get_all/4,
    get_details/1,
    get_recipients/1,
    get_expired_up_to/1,
    delete/1,
    delete/2,
    set_batch_info/2,
    update/3
]).

-type reason() :: any().
-type req_zbin() :: binary().

%% ===================================================================
%% API
%% ===================================================================

-spec get_all(customer_id(), user_id(), pos_integer(), pos_integer()) ->
    {ok, #batch_info{}} | {error, reason()}.
get_all(CustomerUuid, UserId, Skip, Limit) ->
    CustomerUuidSel =
        case CustomerUuid of
            undefined -> [];
            CustomerId -> [{'ci', CustomerId}]
        end,
    UserIdSel =
        case UserId of
            undefined -> [];
            UserId -> [{'ui', UserId}]
        end,
    Selector =
        {'$query',
            bson:document(
                CustomerUuidSel ++ UserIdSel
            )
        },
    Projector = {'rqs', 0},
    case mongodb_storage:find(defers_storage, mt_defers, Selector, Projector, Skip, Limit) of
        {ok, Docs} ->
            {ok, [k_storage_utils:doc_to_mt_batch_info(D) || {_, D} <- Docs]};
        {error, Error} ->
            {error, Error}
    end.

-spec get_details(req_id()) ->
    {ok, #batch_info{}} | {error, reason()}.
get_details(ReqId) ->
    Selector = {'_id', ReqId},
    Projector = {'rqs', 0},
    case mongodb_storage:find_one(defers_storage, mt_defers, Selector, Projector) of
        {ok, Doc} ->
            {ok, k_storage_utils:doc_to_mt_batch_info(Doc)};
        {error, Error} ->
            {error, Error}
    end.

-spec get_recipients(req_id()) ->
    {ok, [#addr{}]} | {error, reason()}.
get_recipients(ReqId) ->
    Selector = {'_id', ReqId},
    Projector = {'_id', 0, 'rqs', 1},
    case mongodb_storage:find_one(defers_storage, mt_defers, Selector, Projector) of
        {ok, Doc} ->
            {ok, build_recipients(Doc)};
        {error, Error} ->
            {error, Error}
    end.

-spec get_expired_up_to(os:timestamp()) ->
    {ok, [{gateway_id(), req_zbin()}]} | {error, reason()}.
get_expired_up_to(Ts) ->
    Selector = {
        'dft', {'$lte', Ts}
    },
    Projector = {
        'dft', 1, 'b', 1, 'rqs', 1
    },
    case mongodb_storage:find(defers_storage, mt_defers, Selector, Projector) of
        {ok, Docs} ->
            {ok, [reformat_req(D) || {_, D} <- Docs]};
        {error, Error} ->
            {error, Error}
    end.

-spec delete(req_id()) -> ok | {error, reason()}.
delete(ReqId) ->
    mongodb_storage:delete(defers_storage, mt_defers, {'_id', ReqId}).

-spec delete(req_id(), gateway_id()) -> ok | {error, reason()}.
delete(ReqId, GtwId) ->
    Selector = {
        '_id', ReqId
    },
    Modifier = {
        '$pull', {
            'rqs', {'gi', GtwId}
        }
    },
    mongodb_storage:upsert(defers_storage, mt_defers, Selector, Modifier).

-spec set_batch_info(#batch_info{}, term()) -> ok | {error, reason()}.
set_batch_info(#batch_info{
    req_id = ReqId,
    customer_id = CustomerId,
    user_id = UserId,
    client_type = ClientType,
    def_time = DefTime,
    req_type = ReqType,
    src_addr = SrcAddr,
    encoding = Encoding,
    body = Body,
    reg_dlr = RegDlr,
    esm_class = EsmClass,
    val_period = ValPeriod,
    req_time = ReqTime,
    recipients = Recipients,
    messages = Messages,
    price = Price
}, Req) ->
    Selector = {
        '_id', ReqId
    },
    Modifier = {
        '$setOnInsert', {
            'ci' , CustomerId,
            'ui' , UserId,
            'ct' , bsondoc:atom_to_binary(ClientType),
            'dft', DefTime,
            'rt' , bsondoc:atom_to_binary(ReqType),
            'sa' , k_storage_utils:addr_to_doc(SrcAddr),
            'e'  , k_storage_utils:encoding_to_binary(Encoding),
            'b'  , Body,
            'rd' , RegDlr,
            'ec' , EsmClass,
            'vp' , ValPeriod,
            'rqt', ReqTime
        },
        '$inc', {
            'rs' , Recipients,
            'ms' , Messages,
            'p'  , Price
        },
        '$push', {
            'rqs', {
                'gi', gateway_id(Req),
                'rq', req_to_zbin(Req)
            }
        }
    },
    mongodb_storage:upsert(defers_storage, mt_defers, Selector, Modifier).

-spec update(req_id(), os:timestamp(), binary()) -> ok | {error, reason()}.
update(ReqId, DefTime, Body) ->
    Selector = {
        '_id', ReqId
    },
    Fields =
        case {DefTime, Body} of
            {undefined, Body} ->
                {'b', Body};
            {DefTime, undefined} ->
                {'dft', DefTime};
            {DefTime, Body} ->
                {'dft', DefTime, 'b', Body}
        end,
    Modifier = {
        '$set', Fields
    },
    mongodb_storage:upsert(defers_storage, mt_defers, Selector, Modifier).

%% ===================================================================
%% Internal
%% ===================================================================

gateway_id(#sms_req_v1{} = Req) ->
    Req#sms_req_v1.gateway_id.

dst_addrs(#sms_req_v1{} = Req) ->
    Req#sms_req_v1.dst_addrs.

reformat_req(Doc) ->
    ReqId = bsondoc:at('_id', Doc),
    DefTime = bsondoc:at('dft', Doc),
    Body = bsondoc:at('b', Doc),
    Reqs = [{bsondoc:at('gi', R), update_req(bsondoc:at('rq', R), DefTime, Body)}
            || R <- bsondoc:at('rqs', Doc)],
    {ReqId, Reqs}.

update_req(ReqZBin, DefTime, Body) ->
    Req = zbin_to_req(ReqZBin),
    Req2 = Req#sms_req_v1{def_time = DefTime, message = Body},
    req_to_zbin(Req2).

build_recipients(Doc) ->
    Reqs = [zbin_to_req(bsondoc:at('rq', R)) || R <- bsondoc:at('rqs', Doc)],
    lists:sort(lists:flatten([dst_addrs(R) || R <- Reqs])).

req_to_zbin(Req) ->
    zlib:compress(term_to_binary(Req)).

zbin_to_req(Bin) ->
    binary_to_term(zlib:uncompress(Bin)).
