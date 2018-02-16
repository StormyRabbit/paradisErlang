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
    hooray.


index_test() ->
    {_, Index} = index:start(),
    {_, Tracker} = tracker:start(),
    Peer = peer:start("peerOne/", Index, Tracker),
    io:format("peer1: ~p~n", [Peer]),
    Peer2 = peer:start("peerTwo/", Index, Tracker),
    io:format("peer2: ~p~n", [Peer2]),

    peer:search_for_file(Peer, "p2.txt", Index, Tracker),

    hooray.