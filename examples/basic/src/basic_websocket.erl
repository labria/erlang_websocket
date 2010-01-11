%%
%% Simple Echo example
%% @author Dave Bryson [http://weblog.miceda.org]
%%
-module(basic_websocket).

-export([start/1, stop/0, loop/1]).

-record(st,
  {sock,
  room}).

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
    "/join " ++ MsgData ->
      WriterState = gen_server:call(Writer, state),
      case  WriterState#st.room of
        undefined ->
          case string:tokens(MsgData, " ") of
            [Room,Nick] -> 
              main_dispatcher:register_user(Writer, Room, Nick);
            _ ->
              client_writer:send_to_client(Writer, "wrong join command format")
          end;
        _ ->
          client_writer:send_to_client(Writer, "you already joined a room")
      end;
    %% Leave room
    "/leave" ->
      client_writer:leave(Writer),
      exit(normal);
    closed ->
      client_writer:leave(Writer),
      exit(normal);
    %% Other messages go here 
    Other ->
      client_writer:message(Writer, Other)
  end.

