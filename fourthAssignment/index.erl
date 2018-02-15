-module(index).
-behaviour(gen_server).
-compile(export_all).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
test(N) -> gen_server:call(?MODULE, {asd, N}).
init([]) -> {ok, []}.
handle_call({add_to_index, FileName, Md5, Size}) -> {reply, FileName, Md5, Size}.
