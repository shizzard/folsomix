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
    handle_autodiscovery(Socket, type_to_atom(Type));

handle_packet(Socket, <<"dump ", Type/binary>>) ->
    handle_dump(Socket, type_to_atom(Type));

handle_packet(Socket, Packet) ->
    gen_tcp:send(Socket, <<"Unknown request: ", Packet/binary>>),
    ok.



handle_autodiscovery(Socket, Type) ->
    TypeMetrics = lists:foldl(fun({Metric, Props}, Acc) ->
        case proplists:get_value(type, Props) of
            Type -> [Metric | Acc];
            _ -> Acc
        end
    end, [], folsom_metrics:get_metrics_info()),
    Message = metrics_to_message(TypeMetrics),
    gen_tcp:send(Socket, Message),
    ok.



handle_dump(Socket, Type) ->
    io:format("Dump of metrics type ~p~n", [Type]),
    gen_tcp:send(Socket, <<"Ok, thanks">>),
    ok.



metrics_to_message(Metrics) ->
    metrics_to_message(Metrics, <<>>).

metrics_to_message([Metric | []], Acc) ->
    <<Acc/binary, (metric_to_binary(Metric))/binary>>;

metrics_to_message([Metric | Metrics], Acc) ->
    metrics_to_message(Metrics, <<Acc/binary, (metric_to_binary(Metric))/binary, "\n">>).



metric_to_binary(Metric) when is_binary(Metric) ->
    Metric;

metric_to_binary(Metric) when is_list(Metric) ->
    list_metric_to_binary(Metric, <<>>);

metric_to_binary(Metric) when is_atom(Metric) ->
    atom_to_binary(Metric, latin1);

metric_to_binary(Metric) when is_integer(Metric) ->
    list_to_binary(integer_to_list(Metric)).



list_metric_to_binary([], Acc) ->
    Acc;

list_metric_to_binary([Metric | List], <<>>) ->
    list_metric_to_binary(List, metric_to_binary(Metric));

list_metric_to_binary([Metric | List], Acc) ->
    list_metric_to_binary(List, <<Acc/binary, ".", (metric_to_binary(Metric))/binary>>).



type_to_atom(<<"counter", _/binary>>) -> counter;
type_to_atom(<<"duration", _/binary>>) -> duration;
type_to_atom(<<"gauge", _/binary>>) -> gauge;
type_to_atom(<<"histogram", _/binary>>) -> histogram;
type_to_atom(<<"history", _/binary>>) -> history;
type_to_atom(<<"meter", _/binary>>) -> meter;
type_to_atom(<<"meter_reader", _/binary>>) -> meter_reader;
type_to_atom(<<"spiral", _/binary>>) -> spiral.
