scotp
=====

A Supercollidder OTP application

Build
-----

    $ rebar3 compile

Run
-----

    $ rebar3 shell


OTP Structure
-----

- SCOTP supervisor 
  * clock supervisor
  * sequencer supervisor
  * repl supervisor


API
-----

### Clock

- start a clock.

when startup, a default clock is created, you can pass an atom or PID to specify a clock.

```erlang
% new clock with name clock1 and 120 bpm
{ok, ClockPid} = clock:new(clock1).

% new clock with name clock2 and 200 bpm
{ok, ClockPid2} = clock:new(clock2, 200).
```

- change bpm

```erlang
% change default clock bpm to 60
clock:change_bpm(60).

% change clock1 bpm to 60
clock:change_bpm(clock1, 60).
```

- schedule a job to run each beat

```erlang
% schedule a job on clock1, set a reference for further unscheduling, 
clock:sched(clock1, funname, fun(ClockMap)-> io:format("~w~n",[ClockMap]) end).

% schedule a job on default clock, set a reference for further unscheduling, 
clock:sched(funname, fun(ClockMap)-> io:format("~w~n",[ClockMap]) end).
```

- unchedule a job


```erlang
% unschedule a job of clock1
clock:unsched(clock1, funname).

% unschedule a job of default clock
clock:unsched(funname).
```

- get clock state 

```erlang
% for clock1
clock:get(clock1).

% for default clock
clock:get().
```

- stop clock

```erlang
% for clock1
clock:stop(clock1).

% for default clock
clock:stop().
```




### Supercollider 

- load a synthdef, the path is expanded

```erlang
sc:load_synthdef("synthdefs/piano.scsyndef").
```

- create a new synth

```erlang
sc:new_synth("piano",#{amp=>1}).
```



TODO
-----

- repeat, mirror, stutter, geom, euclid
- range, xrange
- once, always, at, when, sometimes 




