-module(tracker).
-behaviour(gen_server).
-compile(export_all).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
test(N) -> gen_server:call(?MODULE, {asd, N}).

init([]) -> {ok, []}.

handle_call(N) -> {reply, N};
handle_call({i_have, Md5}) -> {reply, N};
handle_call({i_am_interested_in, Md5}) -> {reply, Md5};
handle_call({who_is_interested, Md5}) -> {reply, Md5}.