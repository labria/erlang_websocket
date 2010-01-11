-module(main_dispatcher).
-compile(export_all).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop(Module) ->
  gen_server:call(Module, stop).

stop() ->
  stop(?MODULE).

state(Module) ->
  gen_server:call(Module, state).

state() ->
  state(?MODULE).

%% Server implementation, a.k.a.: callbacks

init([]) ->
  say("init main_dispatcher", []),
  Tab = ets:new(?MODULE, []),
  {ok, Tab}.


handle_call(stop, _From, State) ->
  say("stopping by ~p, state was ~p.", [_From, State]),
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  say("~p is asking for the state.", [_From]),
  {reply, State, State};

handle_call(_Request, _From, State) ->
  say("call ~p, ~p, ~p.", [_Request, _From, State]),
  {reply, ok, State}.

register_user(ClientPid, Room, Nick) ->
  gen_server:cast(main_dispatcher, {register_user, ClientPid, Room, Nick}).

handle_cast({register_user, Pid, Room, Nick}, State) ->
  % find or create room from ets
  RoomPid = find_or_create_room(Room, State),
  % send it back to the client handler
  room_server:join(RoomPid, Pid, Nick),
  % gen_server:cast(RoomPid, {join, Pid, Nick}),
  gen_server:cast(Pid, {set_room_pid, RoomPid}),
  {noreply, State};

handle_cast(_Msg, State) ->
  say("cast ~p, ~p.", [_Msg, State]),
  {noreply, State}.

find_or_create_room(Room, Tab) ->
  case ets:lookup(Tab, Room) of
    [] -> 
      {ok, Pid} = room_server:start(Room),
      ets:insert(Tab,{Room, Pid}),
      Pid;
    [{Room, Pid}] ->
      Pid
  end.

handle_info(_Info, State) ->
  say("info ~p, ~p.", [_Info, State]),
  {noreply, State}.


terminate(_Reason, _State) ->
  say("terminate ~p, ~p", [_Reason, _State]),
  ok.


code_change(_OldVsn, State, _Extra) ->
  say("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
  {ok, State}.

%% Some helper methods.

say(Format) ->
  say(Format, []).
say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

