-module(room_server).
-compile(export_all).
 
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
%% Public API
-record(st,
  {name,
  users}).


start(Name) ->
  gen_server:start(?MODULE, Name, []).
 
stop(Module) ->
  gen_server:call(Module, stop).
 
stop() ->
  stop(?MODULE).
 
state(Module) ->
  gen_server:call(Module, state).
 
state() ->
  state(?MODULE).
 
%% Server implementation, a.k.a.: callbacks
 
init(Name) ->
  say("init", []),
  Tab = ets:new(?MODULE, []),
  {ok, #st{name=Name, users=Tab}}.
 
 
handle_call(stop, _From, St) ->
  say("stopping by ~p, state was ~p.", [_From, St]),
  {stop, normal, stopped, St};
 
handle_call(state, _From, St) ->
  say("~p is asking for the state.", [_From]),
  {reply, St, St};
 
handle_call(_Request, _From, St) ->
  say("call ~p, ~p, ~p.", [_Request, _From, St]),
  {reply, ok, St}.


join(Pid, ClientPid ,Nickname) ->
  gen_server:cast(Pid, {join, ClientPid, Nickname}). 

leave(Pid, ClientPid) ->
  gen_server:cast(Pid, {leave, ClientPid}).

say(Pid, ClientPid, Msg) ->
  gen_server:cast(Pid, {say, ClientPid, Msg}).


handle_cast({join, Pid, Nickname}, St) ->
  say("~p (~p) is joining the room.", [Nickname, Pid]),
  ets:insert(St#st.users, {Pid, Nickname}),
  % Clients = [Pid|St#st.clients],
  {noreply, St};

handle_cast({leave, Pid}, St) ->
  [{Pid, Nick}] = ets:lookup(St#st.users, Pid),
  ets:delete(St#st.users, Pid),
  say("~p (~p) is leaving the room.", [Nick, Pid]),
  Fun = fun({Client, _}, _) -> client_writer:send_to_client(Client, Nick ++ " left the room"), void end,
  ets:foldl(Fun, [], St#st.users),
  {noreply, St};


handle_cast({say, Pid, Msg}, St) ->
  [{Pid, Nick}] = ets:lookup(St#st.users, Pid),
  Fun = fun({Client, _}, _) -> client_writer:send_to_client(Client, Nick ++ " : "++ Msg), void end,
  ets:foldl(Fun, [], St#st.users),
  % lists:foreach(Fun, St#st.clients),
  {noreply, St};


handle_cast(_Msg, St) ->
  say("cast ~p, ~p.", [_Msg, St]),
  {noreply, St}.
 
 
handle_info(_Info, St) ->
  say("info ~p, ~p.", [_Info, St]),
  {noreply, St}.
 
 
terminate(_Reason, _St) ->
  say("terminate ~p, ~p", [_Reason, _St]),
  ok.
 
 
code_change(_OldVsn, St, _Extra) ->
  say("code_change ~p, ~p, ~p", [_OldVsn, St, _Extra]),
  {ok, St}.
 
%% Some helper methods. 

say(Format) ->
  say(Format, []).
say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
