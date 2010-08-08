-module(ewd).

-export([start/0, stop/0, restart/0, new/0, new/1]).

start() ->
    erlang:set_cookie(node(), 'Yjb5XSNf'),
    application:start(ewd).

stop() ->
    application:stop(ewd).

restart() ->
    application:stop(ewd),
    application:start(ewd).

new() ->
    new(firefox).

new(Type) ->
    ewd_server:new(Type).

stop(Pid) ->
    ok.


get_elem(Pid, elem_name) ->
    ref.

get_text(Pid, Ref) ->
    text.



