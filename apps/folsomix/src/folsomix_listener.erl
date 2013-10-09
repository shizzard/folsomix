-module(folsomix_listener).

-export([start_link/0, init/1]).

-define(LISTEN_OPTS, [{active, false}, binary]).



start_link() ->
    {ok, spawn_link(?MODULE, init, [[]])}.



init([]) ->
    Port = env(port, 6701),
    Opts = lists:filter(fun is_listen_opt_allowed/1, env(listen_opts, [])),
    case gen_tcp:listen(Port, ?LISTEN_OPTS ++ Opts) of
        {error, _Reason} = Error ->
            Error;
        {ok, ListenSocket} ->
            loop(ListenSocket)
    end.



%% Internals



loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {error, _Reason} = Error ->
            Error;
        {ok, Socket} ->
            {ok, _Pid} = folsomix_handler:start(Socket),
            loop(ListenSocket)
    end.



env(Elem, Default) ->
    case application:get_env(folsomix, Elem) of
        undefined -> Default;
        {ok, Value} -> Value
    end.



is_listen_opt_allowed({active, _}) -> false;
is_listen_opt_allowed(list) -> false;
is_listen_opt_allowed(binary) -> false;
is_listen_opt_allowed({packet, _}) -> false;
is_listen_opt_allowed(_) -> true.
