-module(ewd).

-export([start/0, stop/0]).

start() ->
    application:start(ewd).

stop() ->
    application:stop(ewd).

new() ->
    supRef.

stop(Pid) ->
    ok.


get_elem(Pid, elem_name) ->
    ref.

get_text(Pid, Ref) ->
    text.



