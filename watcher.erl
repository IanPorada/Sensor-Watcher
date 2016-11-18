-module(watcher).
-author("Ian Porada").

-export([setup/0, setup_watcher/2]).

% Wait for a message
loop(Sensors) ->
    receive
        {From, Measurement} ->
            io:fwrite("Received Measurement ~p from Sensor ~p~n", [Measurement, From]),
            loop(Sensors);
        {'DOWN', _Ref, process, Pid, Reason} ->
            {_, SensorID} = lists:keyfind(Pid, 1, Sensors),
            io:fwrite("Sensor ~p died for reason ~s~n", [SensorID, Reason]),
            {Sensor, _} = spawn_monitor(sensor, loop, [self(), SensorID]),
            NewSensors = lists:keyreplace(Pid, 1, Sensors, {Sensor, SensorID}),
            io:fwrite("Watcher restarted sensor  ~w~n", [NewSensors]),
            loop(NewSensors)
    end.

% main initialization; passes user input to setup_loop
setup() ->
    {ok, [ N ]} = io:fread("enter number of sensors> ", "~d"),
    if N =< 1 ->
        io:fwrite("setup: range must be at least 2~n", []);
    true ->
        Num_watchers = 1 + ((N - 1) div 10),
        setup_loop(N, Num_watchers)
    end.

% spawn the necessary watchers
setup_loop(Num_sensors, 1) ->
    spawn(watcher, setup_watcher, [0, Num_sensors]);
setup_loop(Num_sensors, Num_watchers) ->
    N = ((Num_sensors - 1) rem 10) + 1,
    spawn(watcher, setup_watcher, [Num_watchers - 1, N]),
    setup_loop(Num_sensors - N, Num_watchers - 1).

% generate sensors and run loop
setup_watcher(ID, Num_sensors) ->
    SensorList = setup_sensors(ID*10, Num_sensors),
    io:fwrite("Watcher started with sensors ~w~n", [SensorList]),
    loop(SensorList).

% spawn sensors
setup_sensors(BaseID, 1) ->
    {Sensor, _} = spawn_monitor(sensor, loop, [self(), BaseID]), 
    [{Sensor, BaseID}];
setup_sensors(BaseID, Num_sensors) ->
    SensorID = BaseID + (Num_sensors - 1),
    {Sensor, _} = spawn_monitor(sensor, loop, [self(), SensorID]),
    [{Sensor, SensorID} | setup_sensors(BaseID, Num_sensors - 1)].
