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
              % gen_server:cast(main_dispatcher, {register, Writer, Room, Nick});
            _ ->
              gen_server:cast(Writer, {send_to_client, "wrong join command format"})
          end;
        _ ->
          gen_server:cast(Writer, {send_to_client, "you already joined a room"})
      end;
    %% Leave room
    "/leave" ->
      gen_server:cast(Writer, leave),
      exit(normal);
    closed ->
      gen_server:cast(Writer, leave),
      exit(normal);
    %% Other messages go here 
    Other ->
      gen_server:cast(Writer, {message, Other})
  end.

