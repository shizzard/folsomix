-module(folsomix_app).

-behaviour(application).

-export([start/2, stop/1, test_run/0]).



%% Interface



start(_StartType, _StartArgs) ->
    folsomix_sup:start_link().



stop(_State) ->
    ok.



test_run() ->
    lists:foreach(fun(App) ->
        application:start(App)
    end, [folsom, folsomix]),
    define_metrics().



%% Internals



define_metrics() ->
    Config = [
        {counter, <<"counter">>, new_counter},
        {duration, <<"duration">>, new_duration},
        {gauge, <<"gauge">>, new_gauge},
        {histogram, <<"histogram">>, new_histogram},
        {history, <<"history">>, new_history},
        {meter, <<"meter">>, new_meter},
        {meter_reader, <<"meter_reader">>, new_meter_reader},
        {spiral, <<"spiral">>, new_spiral}
    ],
    lists:foreach(fun({Atom, Binary, Fun}) ->
        folsom_metrics:Fun(Atom),
        lists:foreach(fun(Metric) ->
            folsom_metrics:Fun([Binary | Metric])
        end, [
            [foo, bar],
            [chocopie, 13],
            [workhost, folsom]
        ])
    end, Config).