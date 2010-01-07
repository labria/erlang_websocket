-module(flash_policy_server).
-export([start_link/1, loop/1, init/1, handle_client/1]).

start_link(Port) ->
  proc_lib:start_link(?MODULE, init, [{self(),Port}]).

init({Parent,Port}) ->
  case gen_tcp:listen(Port, [binary, {packet, raw}, {active, false}]) of
    {ok, Sock} ->
      proc_lib:init_ack(Parent, {ok, self()}),
      loop(Sock);
    {error, _} ->
      exit(could_not_open_sock)
  end.

loop(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, S} ->
      % отдаем переговорщику
      Pid = spawn(?MODULE, handle_client, [S]),
      gen_tcp:controlling_process(S, Pid),
      loop(Listen);
    {error, closed} ->
      exit(closed_socket)
  end.

handle_client(S) ->
  case gen_tcp:recv(S, 0) of
    {ok, _} ->
      gen_tcp:send(S, "<cross-domain-policy><allow-access-from domain=\"*\" to-ports=\"*\" /></cross-domain-policy>"),
      gen_tcp:close(S);
    {error, _} ->
         ok
  end.
