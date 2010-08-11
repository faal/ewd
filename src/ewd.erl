-module(ewd).

-export([start/0, stop/0, restart/0, new/0, new/1, get/2, call/3, cast/3, get_current_url/1, get_title/1]).

-define(SERVER, ewd_server).
-define(NODE, 'WD@localhost').
-define(TIMEOUT, 15000).

start() ->
    erlang:set_cookie(node(), 'Yjb5XSNf'),
    application:start(ewd).

stop() ->
    application:stop(ewd).

stop(Instance) ->
    gen_server:call(Instance, {stop, []}, ?TIMEOUT).

restart() ->
    application:stop(ewd),
    application:start(ewd).

new() ->
    new(firefox).

new(Type) ->
    Args = [{type, Type}],
    gen_server:call(?SERVER, {new, Args}, 15000).



%%--------------------------------------------------------------------
%% @doc
%% Get 
%%
%% @spec new(Instance :: pid(), URL :: string()) -> ok
%%--------------------------------------------------------------------
get(Instance, URL) ->
    gen_server:call(Instance, {get, URL}, ?TIMEOUT).

get_current_url(Instance) ->
    gen_server:call(Instance, {get_current_url, []}, ?TIMEOUT).

get_title(Instance) ->
    gen_server:call(Instance, {get_title, []}, ?TIMEOUT).
call(Pid, Fun, Arg) when is_atom(Pid) ->
    call({Pid, ?NODE}, Fun, Arg);

call(Pid, Fun, Arg) ->
    io:format("sending message~n"),
    erlang:display(Pid),
    erlang:display(Fun),
    erlang:display(Arg),
    Pid ! {self(), Fun, Arg},
    rec(Pid, Fun).

rec({Pid, _Node}, Fun) ->
    receive {Pid, Fun, Res} -> Res end;
rec(Pid, Fun) ->
    receive {Pid, Fun, Res} -> Res end.

cast(Pid, Fun, Arg) ->
    io:format("sending message~n"),
    {Pid, ?NODE} ! {self(), Fun, Arg},
    ok.
