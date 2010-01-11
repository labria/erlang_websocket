-module(basic_writer).
-compile(export_all).
 
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(st,
  {sock,
  room}).
%% Public API
 
start_link(Sock) ->
  gen_server:start_link(?MODULE, [Sock], []).
 
stop(Module) ->
  gen_server:call(Module, stop).
 
stop() ->
  stop(?MODULE).
 
state(Module) ->
  gen_server:call(Module, state).
 
state() ->
  state(?MODULE).
 
%% Server implementation, a.k.a.: callbacks
 
init([Sock]) ->
  say("init basic_writer", []),
  {ok, #st{sock = Sock}}.
 
 
handle_call(stop, _From, St) ->
  say("stopping by ~p, state was ~p.", [_From, St]),
  {stop, normal, stopped, St};
 
handle_call(state, _From, St) ->
  say("~p is asking for the state.", [_From]),
  {reply, St, St};
 
handle_call(_Request, _From, St) ->
  say("call ~p, ~p, ~p.", [_Request, _From, St]),
  {reply, ok, St}.
 
handle_cast({set_room_pid, Pid}, St) ->
  % gen_server:cast(Pid, {join, self()}),
  {noreply, St#st{room = Pid}};

%handle_cast({join, Room}, St) ->
%  gen_server:cast(main_dispatcher, {register, self(), Room}),
%  send_message(St#st.sock, "you joined the room " ++ Data),
%  {noreply, St};

handle_cast({message, Data}, St) ->
  room_server:say(St#st.room, self(), Data),
  {noreply, St};

handle_cast({send_to_client, Data}, St) ->
  send_message(St#st.sock, Data),
  {noreply, St};

handle_cast(leave, St) ->
  room_server:leave(St#st.room, self()),
  % gen_server:cast(St#st.room, {leave, self()}),
  % exit(normal),
  % send_message(St#st.sock, "you left"),
  {stop,normal,St}.

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

send_message(Socket, Data) ->
    gen_tcp:send(Socket, [0] ++ Data ++ [255]).
