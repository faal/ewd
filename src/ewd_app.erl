-module(ewd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = ewd_sup:start_link(),
    %% make sure node is actually started before we return
    timer:sleep(1000),
    ok = ewd:sync(),
    {ok, Pid}.

stop(_State) ->
    exit(whereis(ewd_sup), shutdown).

