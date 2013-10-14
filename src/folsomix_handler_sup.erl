-module(folsomix_handler_sup).

-behaviour(supervisor).

-include("supervisor.hrl").

-export([start_link/0, init/1]).



%% Interface



start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [
        ?CHILD(folsomix_handler, [], temporary, brutal_kill, worker)
    ]}}.

