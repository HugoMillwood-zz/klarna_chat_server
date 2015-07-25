# Klarna chat server solution
Solution for Klarna's assignment to write a chat server in Erlang. Been a while since I wrote this, but it listens on port 6667 and accepts some commands.

Start the server by running:

  `erl`
  
  `c(chat_server).`
  
  `chat_server:start()`
  
  Start the client by running:
  
  `c(tcp_client)`
  
  `tcp_client:start()`
  
  You should see that the server has received the message and that your client received a response.
