-module(peer).
-compile(export_all).
-include_lib("kernel/include/file.hrl").
% {what_have_you, Md5}
% {i_have,[{Start,Stop}]}
% {send_me,MD5,Start,Stop}
% {add_to_index, FileName, Md5, Size}
% {i_have, Md5}


start(FileDirectory, Index, Tracker) -> 
    Map = setup(FileDirectory, Index, Tracker),
    State = maps:put(working_directory, FileDirectory, Map),
    spawn(fun() -> loop(State) end),
    hooray.

setup(FileDirectory, Index, Tracker) ->
    {ok, FileNames} = file:list_dir(FileDirectory),
    loadFiles(FileNames, FileDirectory, Index, Tracker),
    
loadFiles(FileNames, FileDirectory) -> 
    loadFiles(FileNames, FileDirectory, #{}).
loadFiles([], FileDirectory, Result) -> 
    io:format("LOADED FILES: ~p~n", [Result]),
    Result;
loadFiles([File|Files], FileDirectory, Index, Tracker, Result) -> 
    {ok, Bin} = file:read_file(FileDirectory ++ "/" ++ File),
    Md5 = erlang:md5(Bin),
    notify_server(File, Md5, Index, Tracker),
    loadFiles(Files, FileDirectory, Index, Tracker, maps:put(Md5, File, Result)).
                
notify_server(File, Md5, Index, Tracker) ->
    
    Index ! {add_to_index, File, Md5,}

send_file(File, Receiver, Md5, Start, Stop) ->
    Receiver ! {file, File}.
save_file(File, Files, Md5) -> 
    io:format("SAVING FILE: ~p~n", [File]),
    Files:put(Md5, File, Files).
loop(State) -> 
    receive 

    {From, what_have_you, Md5} -> 
        case maps:find(Md5) of
        {ok, _} -> From ! {self(), i_have, [{0,0}]},

        loop(State);
    
    {From, i_have, [{Start, Stop}]} -> 
        loop(State);
    
    {From, send_me, Md5, Start, Stop} -> 
        io:format("FROM: ~p~n", [From]),
        send_file(State, From, Md5, Start, Stop), 
        loop(State);

    {file, File, Md5} ->  
        NewState = save_file(File, State, Md5), 
        loop(NewState);
    {From, add_to_index, FileName, Md5, Size} ->
        loop(State);

    {From, i_have, Md5} ->
        loop(State)
    end.

