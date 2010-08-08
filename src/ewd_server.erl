%%%-------------------------------------------------------------------
%%% @author Fabian <fabian.alenius@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created :  5 Aug 2010 by Fabian <fabian.alenius@gmail.com>
%%%-------------------------------------------------------------------
-module(ewd_server).

-behaviour(gen_server).

%% API
-export([start_link/0, new/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(NODE, 'WD@localhost').

-record(state, {port}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Create a new web_driver instance of Type
%%
%% @spec new(Type :: firefox) -> instance()
%%--------------------------------------------------------------------
new(Type) ->
    Args = [{type, Type}],
    gen_server:call(?SERVER, {new, Args}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    io:format("starting child~n"),
    Dir = filename:dirname(code:priv_dir(ewd)),
    Obj = filename:join(Dir, "java_src/obj/"),
    Lib = filename:join(Dir, "java_src/lib/*"),
    Path = string:join([".", Obj, Lib], ":"),
    Cmd = "java -cp \"" ++ Path ++ "\" WD",
    Port = erlang:open_port({spawn, Cmd}, []),
    {ok, #state{port = Port}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({new, Args}, _From, State) ->
    Pid = call(new, Args),
    {reply, Pid, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    [io:format("Port: ~p~n", [X]) || X <- string:tokens(Data, "\n")],
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("ewd_server terminating~n"),
    cast(stop, undefined),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

call(Fun, Args) ->
    io:format("sending message~n"),
    {server, ?NODE} ! {self(), Fun, test},
    io:format("receiving message~n"),
    receive Msg -> Msg end.

cast(Fun, Args) ->
    io:format("sending message~n"),
    {server, ?NODE} ! {self(), Fun, test}.
