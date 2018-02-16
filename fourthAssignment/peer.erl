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
    spawn(fun() -> loop(FileDirectory) end).

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
    NewMap =  maps:put(Md5, File, Result),
    loadFiles(Files, FileDirectory, Index, Tracker, NewMap).
    
search_for_file(Client, FileName, Index, Tracker) ->
    case index:search_for_file(Index, FileName) of
        {ok, no_result} -> io:format("No such file~n");
        {ok, {Md5, Size}} -> register_with_tracker(Client, Md5, FileName, Tracker)
    end.

register_with_tracker(Client, Directory, Md5, FileName, Tracker) ->
    FilePath = FileDirectory ++ "/" ++ File,
    {ok, Bin} = file:read_file(FilePath),
    tracker:i_am_intrested_in(Tracker, Md5, Client),
    Peers = tracker:who_is_interested(Tracker, Md5),
    requestFile(Client, Md5, Peers, FileName).

requestFile(Client, Md5, [Peer|Peers], FileName) ->
    Peer ! {Client, what_have_you, Md5}.

what_have_you(State, Md5) 

loop(State) -> 
    receive 
    {From, what_have_you, Md5} ->
        From ! what_have_you(State, Md5),
        loop(State);
    {From, i_have, [{Start, Stop}]} ->
        io:format("I HAVE RECEIVED FROM: ~p TO ~p ~n", [From, self()]),
        loop(State);
    {From, send_me, Md5, Start, Stop} -> 
        loop(State)
    end.

