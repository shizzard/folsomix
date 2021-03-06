-module(folsomix).

-export([
    join_lines/1,
    get_metrics_by_type/1,
    any_to_binary/1,
    list_to_binary2/1,
    list_to_binary2/2,
    type_to_atom/1
]).



%% Interface



join_lines(Metrics) ->
    join_lines(Metrics, <<>>).

join_lines([], <<>>) ->
    <<>>;

join_lines([Line | []], Acc) ->
    <<Acc/binary, Line/binary>>;

join_lines([Line | Metrics], Acc) ->
    join_lines(Metrics, <<Acc/binary, Line/binary, "\n">>).




get_metrics_by_type(Type) ->
    lists:foldl(fun({Metric, Props}, Acc) ->
        case proplists:get_value(type, Props) of
            Type -> [Metric | Acc];
            _ -> Acc
        end
    end, [], folsom_metrics:get_metrics_info()).



any_to_binary(Metric) when is_binary(Metric) ->
    Metric;

any_to_binary(Metric) when is_list(Metric) ->
    list_to_binary2(Metric);

any_to_binary(Metric) when is_atom(Metric) ->
    atom_to_binary(Metric, latin1);

any_to_binary(Metric) when is_integer(Metric) ->
    list_to_binary(integer_to_list(Metric));

any_to_binary(Metric) when is_float(Metric) ->
    list_to_binary(io_lib:format("~f", [Metric]));

any_to_binary(Metric) ->
    throw({cannot_convert_to_binary, Metric}).



list_to_binary2(List) ->
    list_to_binary2(List, <<".">>).

list_to_binary2(List, Delimiter) ->
    list_to_binary2(List, Delimiter, <<>>).

list_to_binary2([], _Delimiter, Acc) ->
    Acc;

list_to_binary2([Metric | List], Delimiter, <<>>) ->
    list_to_binary2(List, Delimiter, any_to_binary(Metric));

list_to_binary2([Metric | List], Delimiter, Acc) ->
    list_to_binary2(List, Delimiter, <<Acc/binary, Delimiter/binary, (any_to_binary(Metric))/binary>>).



type_to_atom(<<"counter", _/binary>>) -> counter;
type_to_atom(<<"duration", _/binary>>) -> duration;
type_to_atom(<<"gauge", _/binary>>) -> gauge;
type_to_atom(<<"histogram", _/binary>>) -> histogram;
type_to_atom(<<"history", _/binary>>) -> history;
type_to_atom(<<"meter_reader", _/binary>>) -> meter_reader;
type_to_atom(<<"meter", _/binary>>) -> meter;
type_to_atom(<<"spiral", _/binary>>) -> spiral.