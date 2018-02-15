-module(test).
-compile(export_all).

test_file_loading(Directory, FileName) ->
    {ok, File} = file:read_file(Directory ++ "/" ++ FileName),
    Md5 = erlang:md5(File),
    io:format("~p~n", [Md5]),
    hooray.

test() ->
    Pid1 = peer:start("peerOne/"),
    io:format("PEER1: ~p~n", [Pid1]),
    Pid2 = peer:start(),
    io:format("PEER2: ~p~n", [Pid2]),
    Pid2 ! {Pid1, send_me, 2, 0, 0},
    Pid1 ! {file, "File"},

    hooray.
