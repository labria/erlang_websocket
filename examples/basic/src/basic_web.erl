%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for basic.

-module(basic_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun (Req) ->
      ?MODULE:loop(Req, DocRoot)
  end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
  "/" ++ Path = Req:get(path),
  case Req:get(method) of
    Method when Method =:= 'GET'; Method =:= 'HEAD' ->
    case Path of
      "rooms/join/" ++ Room ->
        join_room(Req, Room);
      "rooms/" ++ Room ->
        enter_room(Req, Room);
      _ ->
        Req:serve_file(Path, DocRoot)
    end;
  'POST' ->
    case Path of
      "rooms/join/" ++ Room ->
        join_room_post(Req, Room);
      _ ->
        Req:not_found()
    end;
  _ ->
    Req:respond({501, [], []})
end.

%% Internal API

enter_room(Req, Room) ->
  % if no cookie for room => redirect to /rooms/join/Rooom
  % else render room
  Nick = proplists:get_value(Room, Req:parse_cookie()),
  case Nick of 
    undefined ->
      Req:respond({302, 
          [{"Location", "/rooms/join/" ++ Room}, 
            {"Content-Type", "text/html; charset=UTF-8"}], 
          ""});
    _ ->
      Req:ok({"text/html", "come in!"})
      % Req:ok({"text/html", erltl:render(Room)});
  end.  

join_room(Req, Room) ->
  Nick = proplists:get_value(Room, Req:parse_cookie()),
  case Nick of 
    undefined ->
      Req:ok({"text/html", "log in, please!"});
    _ ->
      Req:respond({302, 
          [{"Location", "/rooms/" ++ Room}, 
            {"Content-Type", "text/html; charset=UTF-8"}], 
          ""})
  end. 

join_room_post(Req, Room) ->
  % (check nickname?)
  Nick = proplists:get_value("nickname", Req:parse_post()),
  case Nick of
    undefined ->
      Req:respond({302,  
          [{"Location", "/rooms/join/" ++ Room}, 
            {"Content-Type", "text/html; charset=UTF-8"}], 
          ""});
    _ ->
      H = mochiweb_cookies:cookie(Room, Nick, [{path, "/"}]),
      Req:respond({302,
          [{"Location", "/rooms/" ++ Room}, 
            {"Content-Type", "text/html; charset=UTF-8"}, H], 
          ""})
  end.

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
