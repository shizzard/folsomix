-module(folsomix_dump).

-export([handle/2]).



%% Interface



handle(Socket, Type) when gauge == Type; counter == Type ->
    Metrics = folsomix:get_metrics_by_type(Type),
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

handle(Socket, histogram) ->
    Metrics = folsomix:get_metrics_by_type(histogram),
    Lines = lists:foldl(fun(Metric, Acc) ->
        Line = <<
            (folsomix:any_to_binary(Metric))/binary,
            "\t",
            (folsomix:list_to_binary2(folsom_metrics:get_metric_value(Metric), <<" ">>))/binary
        >>,
        [Line | Acc]
    end, [], Metrics),
    gen_tcp:send(Socket, folsomix:join_lines(Lines)),
    ok;

handle(Socket, meter) ->
    Metrics = folsomix:get_metrics_by_type(meter),
    Lines = lists:foldl(fun(Metric, Acc) ->
        MetricValue = folsom_metrics:get_metric_value(Metric),
        Count = proplists:get_value(count, MetricValue),
        One = proplists:get_value(one, MetricValue),
        Five = proplists:get_value(five, MetricValue),
        Fifteen = proplists:get_value(fifteen, MetricValue),
        Day = proplists:get_value(day, MetricValue),
        Mean = proplists:get_value(mean, MetricValue),
        Acceleration = proplists:get_value(acceleration, MetricValue),
        OneToFive = proplists:get_value(one_to_five, Acceleration),
        FiveToFifteen = proplists:get_value(five_to_fifteen, Acceleration),
        OneToFifteen = proplists:get_value(one_to_fifteen, Acceleration),

        Line = <<
            (folsomix:any_to_binary(Metric))/binary,
            "\t",
            (folsomix:list_to_binary2([Count, One, Five, Fifteen, Day, Mean, OneToFive, FiveToFifteen, OneToFifteen], <<" ">>))/binary
        >>,
        [Line | Acc]
    end, [], Metrics),
    gen_tcp:send(Socket, folsomix:join_lines(Lines)),
    ok;

handle(Socket, meter_reader) ->
    Metrics = folsomix:get_metrics_by_type(meter_reader),
    Lines = lists:foldl(fun(Metric, Acc) ->
        MetricValue = folsom_metrics:get_metric_value(Metric),
        One = proplists:get_value(one, MetricValue),
        Five = proplists:get_value(five, MetricValue),
        Fifteen = proplists:get_value(fifteen, MetricValue),
        Mean = proplists:get_value(mean, MetricValue),
        Acceleration = proplists:get_value(acceleration, MetricValue),
        OneToFive = proplists:get_value(one_to_five, Acceleration),
        FiveToFifteen = proplists:get_value(five_to_fifteen, Acceleration),
        OneToFifteen = proplists:get_value(one_to_fifteen, Acceleration),

        Line = <<
            (folsomix:any_to_binary(Metric))/binary,
            "\t",
            (folsomix:list_to_binary2([One, Five, Fifteen, Mean, OneToFive, FiveToFifteen, OneToFifteen], <<" ">>))/binary
        >>,
        [Line | Acc]
    end, [], Metrics),
    gen_tcp:send(Socket, folsomix:join_lines(Lines)),
    ok;

handle(Socket, spiral) ->
    Metrics = folsomix:get_metrics_by_type(spiral),
    Lines = lists:foldl(fun(Metric, Acc) ->
        MetricValue = folsom_metrics:get_metric_value(Metric),
        Count = proplists:get_value(count, MetricValue),
        One = proplists:get_value(one, MetricValue),

        Line = <<
            (folsomix:any_to_binary(Metric))/binary,
            "\t",
            (folsomix:list_to_binary2([Count, One], <<" ">>))/binary
        >>,
        [Line | Acc]
    end, [], Metrics),
    gen_tcp:send(Socket, folsomix:join_lines(Lines)),
    ok;

handle(Socket, _Type) ->
    gen_tcp:send(Socket, <<"%UNSUPPORTED%">>),
    ok.



%% Internals