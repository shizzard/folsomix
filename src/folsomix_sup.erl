-module(folsomix_sup).

-behaviour(supervisor).

-include("supervisor.hrl").

-export([start_link/0, init/1]).



%% Interface



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        ?GENERIC_WORKER(folsomix_listener),
        ?GENERIC_SUPERVISOR(folsomix_handler_sup)
    ]}}.

