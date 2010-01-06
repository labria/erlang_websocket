-module(room_server).
-compile(export_all).
 
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
%% Public API
-record(st,
  {name,
  clients=[]}).


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
  {ok, #st{name=Name}}.
 
 
handle_call(stop, _From, St) ->
  say("stopping by ~p, state was ~p.", [_From, St]),
  {stop, normal, stopped, St};
 
handle_call(state, _From, St) ->
  say("~p is asking for the state.", [_From]),
  {reply, St, St};
 
handle_call(_Request, _From, St) ->
  say("call ~p, ~p, ~p.", [_Request, _From, St]),
  {reply, ok, St}.
 

handle_cast({join, Pid}, St) ->
  say("~p is joining the room.", [Pid]),
  Clients = [Pid|St#st.clients],
  {noreply, St#st{clients=Clients}};

handle_cast({say, Pid, Msg}, St) ->
  Fun = fun(Client) -> send_message(Client, Msg) end,
  lists:foreach(Fun, St#st.clients),
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

send_message(Client, Data) ->
  gen_server:cast(Client,{send_to_client, Data}).
