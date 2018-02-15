-module(index).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, add_to_index/3, get_index_list/0]).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_to_index(FileName, Md5, Size) -> gen_server:call(?MODULE, {add_to_index, FileName, Md5, Size}).
get_index_list() -> gen_server:call(?MODULE, {get_index_list}).


init([]) -> Index = [], {ok, Index}.

% handle_call is invoked in response to gen_server:call
handle_call({add_to_index, FileName, Md5, Size}, _From, Index) ->
	Response = case lists:keyfind(Md5, 2, Index) of
	    {_, _, _} ->
			NewIndex = Index,
			{already_indexed, Md5};
		false ->
			NewIndex = [{FileName, Md5, Size } | Index],
            {file_has_been_indexed, Md5}
	end,
	{reply, Response, NewIndex}.

handle_cast(_Message, Index) -> {noreply, Index}.
handle_info(_Message, Index) -> {noreply, Index}.
terminate(_Reason, _Index) -> ok.
code_change(_OldVersion, Index, _Extra) -> {ok, Index}.