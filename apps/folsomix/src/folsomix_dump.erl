-module(folsomix_dump).

-export([handle/2]).



%% Interface



handle(Socket, counter) ->
    Metrics = folsomix:get_metrics_by_type(counter),
    Lines = lists:foldl(fun(Metric, Acc) ->
        Line = <<
            (folsomix:any_to_binary(Metric))/binary,
            "\t",
            (folsomix:any_to_binary(folsom_metrics:get_metric_value(Metric)))/binary
        >>,
        [Line | Acc]
    end, [], Metrics),
    gen_tcp:send(Socket, folsomix:join_lines(Lines)),
    ok;

handle(Socket, Type) ->
    io:format("Dump of metrics type ~p~n", [Type]),
    gen_tcp:send(Socket, <<"Ok, thanks">>),
    ok.



%% Internals