-module(tracker).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, who_is_interested/2, i_am_intrested_in/3, get_state/1]).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

who_is_interested(Pid, Md5) -> gen_server:call(Pid, {who_is_interested, Md5}).
i_am_intrested_in(Pid, Md5, Peer) -> gen_server:call(Pid, {i_am_intrested_in, Md5, Peer}).
get_state(Pid) -> gen_server:call(Pid, {get_state}).

init([]) -> State = #{}, {ok, State}.

handle_call({who_is_interested, Md5}, _From, State) ->
	Response = case maps:is_key(Md5, State) of
	    true ->
        	NewState = State,
            maps:get(Md5, State);
		false ->
            NewState = State,
            {unknown_file, Md5}
	end,
	{reply, Response, NewState};

handle_call({get_state}, _From, State) ->
	{reply, State, State};

handle_call({i_am_intrested_in, Md5, Peer}, _From, State) ->
    % intrest == has part of file, or non of the file
	io:format("~n Intrest received from ~p concerning ~n~p", [Peer, Md5]),
	Response = case maps:is_key(Md5, State) of
	    true ->
            List = maps:get(Md5, State),
			NewState = maps:update(Md5, [Peer|List], State),
			{intrest_added, Md5};
		false ->
			NewState = maps:put(Md5, [Peer], State),
            {file_has_been_indexed, Md5}
	end,
	{reply, Response, NewState}.

handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.