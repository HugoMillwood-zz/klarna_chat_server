% Written by Hugo Millwood

% Module declaration
-module(chat_server).

% Exports (chat_server)
-export([start/0]).

% Defines
-define(PORT, 6667).

% chat_server functions

start() ->
  AllConnections = [],
  {ok, Listen} = gen_tcp:listen(?PORT, [{active,true}, binary]),
  ConnectionManager = spawn(fun() -> connectionManager(AllConnections) end),
  spawn(fun() -> connectionListener(Listen, ConnectionManager) end).

connectionListener(Listen, ConnectionManager) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  io:format("New Socket: ~p~n", [Socket]),
  {ok, Peer} = inet:peername(Socket),
  ConnectionManager ! {addConnection, Socket},
  spawn(fun() -> connectionListener(Listen, ConnectionManager) end),
  loop(Socket, Peer, ConnectionManager, <<>>).

loop(Socket, Peer, ConnectionManager, BinaryBuffer) ->
  ConnectionManager ! {getConnections, self()},
  receive
    {connections, AllConnections} ->
      {ok, got_connections}
  end,

  receive
    {tcp, Socket, ReceivedBinary} ->
      CurrentBinaryBuffer = <<BinaryBuffer/binary, ReceivedBinary/binary>>,
      loop(Socket, Peer, ConnectionManager, processBuffer(CurrentBinaryBuffer, Socket, AllConnections, ConnectionManager));
    {tcp_closed, Socket} ->
      io:format("Disconnecting Socket: ~p~n", [Socket]),
      ConnectionManager ! {removeConnection, Socket}
  end.

processBuffer(BinaryBuffer, Socket, AllConnections, ConnectionManager) ->
  case binary:match(BinaryBuffer, <<"\r">>) of
    {_Position, _Instances} ->
      [FirstValidMessage, RemainingBuffer] = binary:split(BinaryBuffer, <<"\r">>),
      case checkIfCommand(FirstValidMessage) of
        {true, Command} -> 
          parseCommand(Command, Socket, AllConnections, ConnectionManager);
        false ->
          broadcastMessage(FirstValidMessage, Socket, AllConnections)
      end,
      io:format("Server received Message = ~p~n", [FirstValidMessage]),
      processBuffer(RemainingBuffer, Socket, AllConnections, ConnectionManager);
    nomatch ->
      io:format("Remaining buffer size is = ~p~n", [erlang:byte_size(BinaryBuffer)]),
      case erlang:byte_size(BinaryBuffer) > 10000 of
        true ->
          io:format("Buffer is too large, closing socket = ~p~n", [Socket]),
          ConnectionManager ! {removeConnection, Socket};
        false ->
          BinaryBuffer
      end
  end.

checkIfCommand(Message) ->
  case Message of
    <<"$\\", Command/binary>> ->
      {true, Command};
    <<Message/binary>> ->
      false
  end.
  
broadcastMessage(_Message, _From, []) ->
  {ok, message_sent};

broadcastMessage(Bin, From, AllConnections) ->
  [FirstConnection|OtherConnections] = AllConnections,
  gen_tcp:send(FirstConnection, Bin), 
  broadcastMessage(Bin, From, OtherConnections). 

parseCommand(Command, Socket, AllConnections, ConnectionManager) ->
  case Command of
    <<"man">> ->
      gen_tcp:send(Socket, <<"Available commands are: man, users, kickIP (follwed by a space and a valid IP-address) and leave">>);
    <<"kickIP ", IP/binary>> ->
      ConnectionManager ! {removeConnection, getSocket(AllConnections, IP, Socket)};
    <<"leave">> ->
      ConnectionManager ! {removeConnection, Socket};
    <<"users">> ->
      Peers = getPeers(AllConnections, ""),
      gen_tcp:send(Socket, Peers);
    Command ->
      io:format("Received invalid command = ~p~n", [Command]),
      {error, invalid_command}
  end.

getSocket([], _IPString, _Socket) ->
  {error, no_socket};

getSocket(AllConnections, IPString, Socket) ->
  {ok, IP} = inet_parse:address(erlang:binary_to_list(IPString)),
  [FirstConnection|OtherConnections] = AllConnections,
  %{ok, {TempIP, TempPort}} = inet:peername(FirstConnection),
  case inet:peername(FirstConnection) of
    {ok, {IP, _Port}} ->
      FirstConnection;
    {ok, {_NotTheIP, _Port}} ->
      getSocket(OtherConnections, IPString, Socket)
  end.

getPeers([], PeerString) ->
  PeerString;

getPeers(AllConnections, _PeerString) ->
  [FirstConnection|OtherConnections] = AllConnections,
  {ok, {IP, Port}} = inet:peername(FirstConnection),
  getPeers(OtherConnections, "IP: " ++ inet_parse:ntoa(IP) ++ " Port: " ++ erlang:integer_to_list(Port)).

connectionManager(Connections) ->
  receive
    {addConnection, Connection} ->
      connectionManager(Connections ++ [Connection]);
    {removeConnection, Connection} ->
      connectionManager(Connections -- [Connection]);
    {getConnections, PID} ->
      PID ! {connections, Connections}
  end,
  connectionManager(Connections).
