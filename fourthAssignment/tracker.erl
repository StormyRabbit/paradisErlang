-module(tracker).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, who_is_interested/1, i_am_intrested_in/1]).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

who_is_interested(Md5) -> gen_server:call(?MODULE, {who_is_interested, Md5}).
i_am_intrested_in(Md5) -> gen_server:call(?MODULE, {i_am_intrested_in, Md5}).


init([]) -> State = #{}, {ok, State}.

handle_call({who_is_interested, Md5}, _From, State) ->
	Response = case maps:is_key(Md5, State) of
	    true ->
        	NewState = State,
            List = maps:get(Md5, State),
			{intrestedList, Md5, List};
		false ->
            NewState = State,
            {unknown_file, Md5}
	end,
	{reply, Response, NewState};

handle_call({i_am_intrested_in, Md5}, _From, State) ->
    % intrest == has part of file, or non of the file
	Response = case maps:is_key(Md5, State) of
	    true ->
            List = maps:get(Md5, State),
			NewState = maps:update(Md5, [_From|List], State),
			{intrest_added, Md5};
		false ->
			NewState = maps:put(Md5, [_From], State),
            {file_has_been_indexed, Md5}
	end,
	{reply, Response, NewState}.

handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.