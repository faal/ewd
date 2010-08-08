%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Erlang module for automatically reloading modified modules
%% during development.

-module(reloader).
-author("Matthew Dempsky <matthew@mochimedia.com>").

-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).
-export([start/0, start_link/0]).
-export([stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([all_changed/0]).
-export([is_changed/1]).
-export([reload_modules/1]).
-record(state, {last, tref}).

%% External API

%% @spec start() -> ServerRet
%% @doc Start the reloader.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @spec start_link() -> ServerRet
%% @doc Start the reloader.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Stop the reloader.
stop() ->
    gen_server:call(?MODULE, stop).

%% gen_server callbacks

%% @spec init([]) -> {ok, State}
%% @doc gen_server init, opens the server in an initial state.
init([]) ->
    {ok, TRef} = timer:send_interval(timer:seconds(1), doit),
    {ok, #state{last = stamp(), tref = TRef}}.

%% @spec handle_call(Args, From, State) -> tuple()
%% @doc gen_server callback.
handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

%% @spec handle_cast(Cast, State) -> tuple()
%% @doc gen_server callback.
handle_cast(_Req, State) ->
    {noreply, State}.

%% @spec handle_info(Info, State) -> tuple()
%% @doc gen_server callback.
handle_info(doit, State) ->
    Now = stamp(),
    doit(State#state.last, Now),
    {noreply, State#state{last = Now}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc gen_server termination callback.
terminate(_Reason, State) ->
    {ok, cancel} = timer:cancel(State#state.tref),
    ok.


%% @spec code_change(_OldVsn, State, _Extra) -> State
%% @doc gen_server code_change callback (trivial).
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%% @spec reload_modules([atom()]) -> [{module, atom()} | {error, term()}]
%% @doc code:purge/1 and code:load_file/1 the given list of modules in order,
%%      return the results of code:load_file/1.
reload_modules(Modules) ->
    [begin code:purge(M), code:load_file(M) end || M <- Modules].

%% @spec all_changed() -> [atom()]
%% @doc Return a list of beam modules that have changed.
all_changed() ->
    [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].

%% @spec is_changed(atom()) -> boolean()
%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, returns false otherwise.
is_changed(M) ->
    try
        module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
    catch _:_ ->
            false
    end.

%% Internal API

module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam),
    Vsn;
module_vsn(L) when is_list(L) ->
    {_, Attrs} = lists:keyfind(attributes, 1, L),
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
    Vsn.

doit(From, To) ->
    [case file:read_file_info(Filename) of
         {ok, #file_info{mtime = Mtime}} when Mtime >= From, Mtime < To ->
             reload(Module, Filename);
         {ok, _} ->
             unmodified;
         {error, enoent} ->
             %% The Erlang compiler deletes existing .beam files if
             %% recompiling fails.  Maybe it's worth spitting out a
             %% warning here, but I'd want to limit it to just once.
             gone;
         {error, Reason} ->
             io:format("Error reading ~s's file info: ~p~n",
                       [Filename, Reason]),
             error
     end || {Module, Filename} <- code:all_loaded(), is_list(Filename)].

reload(Module, Filename) ->
    io:format("~n - Reloading ~p ...~n", [Module]),
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            test(Module),
            dialyze(Filename),
            xref(Module);
        {error, Reason} ->
            io:format(" fail: ~p.~n", [Reason]),
            error
    end.


test(Module) ->
    case erlang:function_exported(Module, test, 0) of
        true ->
            case catch Module:test() of
                ok ->
                    io:format(" - Eunit: OK~n"),
                    reload;
                Reason ->
                    io:format(" - Eunit: fail: ~p.~n", [Reason]),
                    reload_but_test_failed
            end;
        false ->
            io:format(" - Eunit: test() not found.~n"),
            reload
    end.

dialyze(Filename) ->
    case dialyzer:run([{files, [Filename]}]) of
        [] ->
            io:format(" - Dialyzer: OK~n");
        Warnings ->
            io:format(" - Dialyzer: Warning~n"),
            lists:foreach(fun(W) ->
                                  S = dialyzer:format_warning(W),
                                  io:format(S)
                          end, Warnings)
    end.

xref(Module) ->
    io:format(" - Xref: "),
    xref_print(xref:m(Module)).

xref_print([{deprecated,[]},{undefined,[]},{unused,[]}]) ->
    io:format("OK~n");

xref_print([Depreceated, Undefined, Unused]) ->
    io:format("Warning~n"),
    Print = fun({Type, Calls}) ->
                    case Calls of
                        [] ->
                            ok;
                        Calls ->
                            io:format("Warning: ~p function calls~n", [Type]),
                            [io:format("Call to ~p:~p/~p in ~p:~p/~p~n", [M2, F2, A2, M1, F1, A1]) 
                             || {{M1, F1, A1}, {M2, F2, A2}} <- Calls]
                    end
            end,
    [Print(X) || X <- [Depreceated, Undefined, Unused]].

stamp() ->
    erlang:localtime().

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
