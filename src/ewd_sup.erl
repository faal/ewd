
-module(ewd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    io:format("Starting supervisor~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [server_spec()]}}.

child(I, Type) ->
    {I, {I, start_link, []}, permanent, 5000, Type, [I]}.

server_spec() ->
    child(ewd_server, worker).

