-module(folsomix_app).

-behaviour(application).

-export([start/2, stop/1]).



%% Interface



start(_StartType, _StartArgs) ->
    folsomix_sup:start_link().

stop(_State) ->
    ok.
