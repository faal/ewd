-module(ewd_elem).



-include("../include/ewd.hrl").

-export([get_text/2]).


get_text(Instance, Elem) ->
    gen_server:call(Instance, {get_text, Elem}, ?TIMEOUT).


-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {setup,
     fun ewd:start/0,
     fun(_) -> ewd:stop() end,
     [{timeout, 60, [?_test(t_get_text())]}]}.

t_get_text() ->
    WD = ewd:new(firefox),
    ewd:get(WD, "http://www.reddit.com"),
    {ok, Elem} = ewd:elem_by_id(WD, "siteTable"),
    Text = get_text(WD, Elem),
    ewd:quit(WD),
    ?debugMsg(Elem),
    ?debugMsg(Text).

