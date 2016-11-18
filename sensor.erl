-module(sensor).
-author("Ian Porada").

-export([loop/2]).

% crash or send measurement to watcher
loop(Pid, ID) ->
    Measurement = rand:uniform(11),
    if Measurement == 11 ->
        exit(anomalous_reading);
    true ->
        Pid ! {ID, Measurement}
    end,
    Sleep_time = rand:uniform(10000),
    timer:sleep(Sleep_time),
    loop(Pid, ID).
