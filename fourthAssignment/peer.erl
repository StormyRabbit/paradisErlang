-module(peer).
-compile(export_all).
% {what_have_you, Md5}
% {i_have,[{Start,Stop}]}
% {send_me,MD5,Start,Stop}
% {add_to_index, FileName, Md5, Size}
% {i_have, Md5}


start() -> spawn(fun() -> loop(#{}) end).

add_file(File) -> {ok, Data} = file:read_file(File).

send_file(Files, Receiver, Md5, Start, Stop) ->    
    Receiver ! maps:get(Files, Md5).

loop(Files) -> 
    receive 

        {From, what_have_you, Md5} -> true, loop(Files);
        
        {From, i_have, [{Start, Stop}]} -> true, loop(Files);
        
        {From, send_me, Md5, Start, Stop} -> send_file(Files, From, Md5, Start, Stop), loop(Files);

        {From, add_to_index, FileName, Md5, Size} -> true, loop(Files);

        {From, i_have, Md5} -> true, loop(Files)
    
    end.

