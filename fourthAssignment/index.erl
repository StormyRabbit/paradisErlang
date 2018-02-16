-module(index).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, add_to_index/4, get_index_list/1, search_for_file/2]).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_to_index(Pid, FileName, Md5, Size) -> gen_server:call(Pid, {add_to_index, FileName, Md5, Size}).
get_index_list(Pid) -> gen_server:call(Pid, {print_index}).
search_for_file(Pid, FileName) -> gen_server:call(Pid, {search_for_file, FileName}).

init([]) -> Index = #{}, {ok, Index}.

% handle_call is invoked in response to gen_server:call
handle_call({add_to_index, FileName, Md5, Size}, _From, Index) ->
	Response = case maps:is_key(Md5, Index) of
	    true ->
			NewIndex = Index,
			{already_indexed, Md5};
		false ->
			NewIndex = maps:put(FileName, {Md5, Size}, Index),
            {file_has_been_indexed, Md5}
	end,
	{reply, Response, NewIndex};
handle_call({print_index}, _From, Index) ->
	{reply, Index, Index};
handle_call({search_for_file, FileName}, _From, Index) ->
	Response = case maps:find(FileName, Index) of 
		{ok, Md5} -> {ok, Md5};
		_ -> {ok, no_result}
	end,
	{reply, Response, Index}.


handle_cast(_Message, Index) -> {noreply, Index}.
handle_info(_Message, Index) -> {noreply, Index}.
terminate(_Reason, _Index) -> ok.
code_change(_OldVersion, Index, _Extra) -> {ok, Index}.