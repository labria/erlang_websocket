-module(flash_policy_server).
-export([start/1, loop/1, init/1, handle_client/1]).

start(Port) ->
  spawn_link(?MODULE, init, [Port]).

init(Port) ->
  {ok, Sock} = gen_tcp:listen(Port, [binary, {packet, raw}, {active, false}]),
  loop(Sock).

loop(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, S} ->
      % отдаем переговорщику
      Pid = spawn(?MODULE, handle_client, [S]),
      gen_tcp:controlling_process(S, Pid),
      loop(Listen);
    {error, Err} ->
      loop(Listen)
  end.

handle_client(S) ->
  case gen_tcp:recv(S, 0) of
    {ok, _} ->
      gen_tcp:send(S, "<cross-domain-policy><allow-access-from domain=\"*\" to-ports=\"*\" /></cross-domain-policy>"),
      gen_tcp:close(S);
    {error, _} ->
         ok
  end.
