-module(ewd).

-export([start/0, stop/0, restart/0, new/0, new/1, get/2, call/3, cast/3, get_current_url/1, get_title/1, close/1, quit/1, get_page_src/1, get_window/1, get_windows/1, back/1, sync/0]).

-export([forward/1, refresh/1, target_window/2]).

-define(SERVER, ewd_server).
-define(NODE, 'WD@localhost').
-define(TIMEOUT, 15000).

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
    gen_server:call(?SERVER, {new, Type}, 15000).


sync() ->
    gen_server:call(?SERVER, sync, ?TIMEOUT).

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

close(Instance) ->
    gen_server:call(Instance, {close, []}, ?TIMEOUT).

quit(Instance) ->
    gen_server:call(Instance, {quit, []}, ?TIMEOUT).

get_page_src(Instance) ->
    gen_server:call(Instance, {get_page_src, []}, ?TIMEOUT).

get_window(Instance) ->
    gen_server:call(Instance, {get_window, []}, ?TIMEOUT).

get_windows(Instance) ->
    gen_server:call(Instance, {get_windows, []}, ?TIMEOUT).

back(Instance) ->
    gen_server:call(Instance, {back, []}, ?TIMEOUT).

forward(Instance) ->
    gen_server:call(Instance, {forward, []}, ?TIMEOUT).

refresh(Instance) ->
    gen_server:call(Instance, {refresh, []}, ?TIMEOUT).

target_window(Instance, Window) ->
    gen_server:call(Instance, {target_window, Window}, ?TIMEOUT).

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
