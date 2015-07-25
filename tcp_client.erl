-module(tcp_client).
-export([start/0]).

start() ->
  {ok, Socket} = gen_tcp:connect({127,0,0,1}, 6667, [binary, {active, false}]),
  gen_tcp:send(Socket, "$\\man\rThis is a message\r$\\users\r"),
  loop(Socket).

loop(Socket) ->
  Response = gen_tcp:recv(Socket, 0),
  io:format("Response is = ~p~n", [Response]),
  loop(Socket).
  
