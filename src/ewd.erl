-module(ewd).

-export([start/0, stop/0, restart/0, new/0, new/1, get/2, call/3, cast/3, 
	 current_url/1, title/1, close/1, quit/1, page_src/1, 
	 window/1, windows/1, back/1, sync/0]).

-export([forward/1, refresh/1, target_window/2]).

-export([elem_by_id/2, elems_by_id/2]).

-include("../include/ewd.hrl").

-define(SERVER, ewd_server).
-define(NODE, 'WD@localhost').

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
    gen_server:call(?SERVER, {new, Type}, ?TIMEOUT).

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

current_url(Instance) ->
    gen_server:call(Instance, {current_url, []}, ?TIMEOUT).

title(Instance) ->
    gen_server:call(Instance, {title, []}, ?TIMEOUT).

close(Instance) ->
    gen_server:call(Instance, {close, []}, ?TIMEOUT).

quit(Instance) ->
    gen_server:call(Instance, {quit, []}, ?TIMEOUT).

page_src(Instance) ->
    gen_server:call(Instance, {page_src, []}, ?TIMEOUT).

window(Instance) ->
    gen_server:call(Instance, {window, []}, ?TIMEOUT).

windows(Instance) ->
    gen_server:call(Instance, {windows, []}, ?TIMEOUT).

back(Instance) ->
    gen_server:call(Instance, {back, []}, ?TIMEOUT).

forward(Instance) ->
    gen_server:call(Instance, {forward, []}, ?TIMEOUT).

refresh(Instance) ->
    gen_server:call(Instance, {refresh, []}, ?TIMEOUT).

target_window(Instance, Window) ->
    gen_server:call(Instance, {target_window, Window}, ?TIMEOUT).


elem_by_id(Instance, ID) ->
    gen_server:call(Instance, {elem_by_id, ID}, ?TIMEOUT).

elems_by_id(Instance, ID) ->
    gen_server:call(Instance, {elems_by_id, ID}, ?TIMEOUT).

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


-include_lib("eunit/include/eunit.hrl").


all_test_() ->
    {setup,
     fun start/0,
     fun(_) -> stop() end,
     [{timeout, 60, [?_test(t_find_elem())]},
      {timeout, 60, [?_test(t_find_elems())]},
      {timeout, 60, [?_test(t_navigation())]}]
    }.
    

t_find_elem() ->
    WD = new(firefox),
    get(WD, "http://www.reddit.com"),
    %% This test should be improved
    {ok, _Elem} = elem_by_id(WD, "header"),
    {ok, _Elem2} = elem_by_id(WD, "header"),
    {ok, _Elem3} = elem_by_id(WD, "header"),
    {ok, _Elem4} = elem_by_id(WD, "header"),
    quit(WD).

t_find_elems() ->
    WD = new(firefox),
    get(WD, "http://www.reddit.com"),
    {ok, _Elem} = elems_by_id(WD, "header"),
    quit(WD).

    
t_navigation() ->
    WD = new(firefox),
    URL = "http://www.google.com/",
    get(WD, URL),
    ?assert(URL =:= current_url(WD)),
    ?assert("Google" =:= title(WD)),
    EmptyUrl = "http://www.duke.edu/~zjt3/",
    get(WD, EmptyUrl),
    Src = "<html><head>\n\n<title>Blank HTML Page</title>\n</head><body>\n\n"
	"<center>\nBlank HTML Page\n</center>\n\n</body></html>",
    ?assert(Src =:= page_src(WD)),
    back(WD),
    %% should be back on google.com
    ?assert(URL =:= current_url(WD)),
    forward(WD),
    %% should be at emptypage
    ?assert(EmptyUrl =:= current_url(WD)),
    refresh(WD),
    %% test setting window to current window
    Window = window(WD),
    target_window(WD, Window),
    ?assert(Window =:= window(WD)),
    ?assert([Window] =:= windows(WD)),
    quit(WD).
