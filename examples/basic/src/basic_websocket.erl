%%
%% Simple Echo example
%% @author Dave Bryson [http://weblog.miceda.org]
%%
-module(basic_websocket).

-export([start/1, stop/0, loop/1]).

start(Options) ->
  Loop = fun (WebSocket) ->
      ?MODULE:loop(WebSocket)
  end,
  mochiweb_websocket:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
  mochiweb_websocket:stop(?MODULE).


loop(State) ->

  {WebSocket, {ok, Writer}} = State,
  %% Get the data sent from the client 
  Data = WebSocket:get_data(),

  %% Our example...
  case Data of
    %% Join room
    "join " ++ MsgData ->
      gen_server:cast(Writer, {join, MsgData});
    %% Leave room
    "leave" ->
      gen_server:cast(Writer, leave);
    %% Other messages go here 
    Other ->
      gen_server:cast(Writer, {message, Other})
  end.

