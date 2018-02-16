-module(peer).
-compile(export_all).
-include_lib("kernel/include/file.hrl").
% {what_have_you, Md5}
% {i_have,[{Start,Stop}]}
% {send_me,MD5,Start,Stop}
% {i_have, Md5}


start(FileDirectory, Index, Tracker) -> 
    Map = setup(FileDirectory, Index, Tracker),
    State = maps:put(working_directory, FileDirectory, Map),
    spawn(fun() -> loop(State) end).

setup(FileDirectory, Index, Tracker) ->
    {ok, FileNames} = file:list_dir(FileDirectory),
    loadFiles(FileNames, FileDirectory, Index, Tracker, #{}).

loadFiles([], _, _,_ , Result) -> 
    Result;
loadFiles([File|Files], FileDirectory, Index, Tracker, Result) -> 
    FilePath = FileDirectory ++ "/" ++ File,
    {ok, Bin} = file:read_file(FilePath),
    Size = filelib:file_size(FilePath),  
    Md5 = erlang:md5(Bin),
    index:add_to_index(Index, File, Md5, Size),
    NewMap =  maps:put(Md5, {File, 0, Size}, Result),
    loadFiles(Files, FileDirectory, Index, Tracker, NewMap).


register_with_tracker(Client, FileDirectory, Tracker) ->
    {ok, FileNames} = file:list_dir(FileDirectory),
    load_files_to_tracker(Client, FileNames, FileDirectory, Tracker).
load_files_to_tracker(_, [], _, _) -> ok;
load_files_to_tracker(Client, [File|Files], FileDirectory, Tracker) ->
    {ok, Bin} = file:read_file(FileDirectory ++ "/" ++ File),
    Md5 = erlang:md5(Bin),    
    tracker:i_am_intrested_in(Tracker, Md5, Client).


    
search_for_file(Client, FileName, Index, Tracker) ->
    case index:search_for_file(Index, FileName) of
        {ok, no_result} -> io:format("No such file~n");
        {ok, {Md5, Size}} -> register_with_tracker(Client, Md5, Size, FileName, Tracker)
    end.

register_with_tracker(Client, Md5, Size, FileName, Tracker) ->
    tracker:i_am_intrested_in(Tracker, Md5, Client),
    Peers = tracker:who_is_interested(Tracker, Md5),
    io:format("~nPEERS: ~p~n", [Peers]),
    requestFile(Client, Md5, Size, Peers, FileName).

requestFile(Client, Md5, Size, Peers, FileName) ->
    Peers = spawn_workers(Client, Peers, Md5),
    requestFile(Client, Md5, Peers, FileName).

spawn_workers(Client, [Peer|Peers], Md5) ->
    spawn(fun() -> what_have_you(Client, Peer, Md5)),

what_have_you(Client, Peer, Md5) ->
    Peer ! {Client, what_have_you, Md5},
    receive {}

what_have_you(State, Md5) -> 
    case maps:find(Md5, State) of
        {ok, {_, Start, Stop}} -> {Start, Stop};
        _ -> {0,0}
    end.
        
loop(State) -> 
    receive 
    {From, what_have_you, Md5} ->
        Response = what_have_you(State, Md5),
        From ! {self(), i_have, [Response]},
        loop(State);
    {From, i_have, [{Start, Stop}]} ->
        io:format("~p HAS DATA FROM ~p TO ~p~n", [From, Start, Stop]),
        loop(State);
    {From, send_me, Md5, Start, Stop} -> 
        loop(State)
    end.

