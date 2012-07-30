-ifndef(crud_specs_hrl).
-define(crud_specs_hrl, included).

-include("crud.hrl").

-spec init() -> {ok, #specs{}}.
-spec create([{atom(), term()}]) -> {ok, response()} | {error, reason()}.
-spec read([{atom(), term()}]) -> {ok, response()} | {error, reason()}.
-spec update([{atom(), term()}]) -> {ok, response()} | {error, reason()}.
-spec delete([{atom(), term()}]) -> {ok, response()} | {error, reason()}.

-endif.
