-module(folsomix_handler).

-export([start/1, start_link/1, run/1]).



%% Interface



start(Socket) ->
    supervisor:start_child(folsomix_handler_sup, [Socket]).



start_link(Socket) ->
    {ok, spawn_link(?MODULE, run, [Socket])}.



run(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {error, closed} ->
            exit(normal);
        {error, _Reason} = Error ->
            exit(Error);
        {ok, Packet} ->
            handle_packet(Socket, Packet),
            gen_tcp:close(Socket),
            exit(normal)
    end.



%% Internals



handle_packet(Socket, <<"autodiscovery ", Type/binary>>) ->
    folsomix_autodiscovery:handle(Socket, folsomix:type_to_atom(Type));

handle_packet(Socket, <<"dump ", Type/binary>>) ->
    folsomix_dump:handle(Socket, folsomix:type_to_atom(Type));

handle_packet(Socket, Packet) ->
    gen_tcp:send(Socket, <<"Unknown request: ", Packet/binary>>),
    ok.
