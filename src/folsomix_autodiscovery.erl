-module(folsomix_autodiscovery).

-export([handle/2]).



%% Interface



handle(Socket, Type) ->
    Lines = lists:map(fun(Metric) ->
        folsomix:any_to_binary(Metric)
    end, folsomix:get_metrics_by_type(Type)),
    gen_tcp:send(Socket, folsomix:join_lines(Lines)),
    ok.



%% Internals


