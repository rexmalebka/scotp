-module(clock).
-export([
         new/1,
         new/2,
         change_bpm/2,
         sched/3,
         get/1,
         stop/1
        ]).

new(ClockName) ->
  new(ClockName, 120).

new(ClockName, Bpm) when 
    is_atom(ClockName) and is_number(Bpm) ->
  Clocks = clock_sup:list_clock(),
  FilteredClock = lists:filter(fun({Id, _, _, _})->
                                 Id == ClockName
                             end,
                             Clocks),

  {ok, ClockPid} = case length(FilteredClock) of
    0 ->
      clock_sup:new_clock(ClockName, Bpm);
    _ ->
      {ClockName, Pid, _, _} = lists:nth(1,FilteredClock),
      {ok, Pid}
  end,
  gen_server:cast(ClockPid, start),
  {ok, ClockPid}.

change_bpm(Clock, Bpm) when
    Bpm >= 0->
  gen_server:call(Clock, {bpm, Bpm}),
  gen_server:cast(Clock, start),
  {ok, Bpm}.

sched(Clock, Ref, Func)  when is_function(Func) ->
  io:format("ched diz shit ~w",[{Ref,Func}]),
  gen_server:cast(Clock, {sched, {Ref, Func}}).

get(Clock) ->
  gen_server:call(Clock, get).  

stop(Clock) ->
  gen_server:call(Clock, stop).
