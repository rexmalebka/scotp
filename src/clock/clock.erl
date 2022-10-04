-module(clock).
-export([
	 new/1,
	 new/2,

	 change_bpm/1,
	 change_bpm/2,

	 sched/2,
	 sched/3,

	 unsched/1,
	 unsched/2,

	 get/0,
	 get/1,

	 start/0,
	 start/1,

	 stop/0,
	 stop/1
	]).

check_existing(ClockName)->
	Clocks = maps:from_list(
		   [{Name, Pid} || {Name, Pid, _, _ }<- clock_sup:list_clocks()]
		  ),
	case 
		maps:get(ClockName, Clocks, noclock) of
		noclock ->
			{error, noclock};
		Pid -> 
			{ok, Pid}
	end.

new(ClockName) ->
	new(ClockName, 120).

new(ClockName, Bpm) when 
	  is_atom(ClockName); Bpm > 0 ->
	case check_existing(ClockName) of
		{ok, Pid} -> 
			{ok, Pid};
		{error, noclock} ->
			{ok,Pid} = clock_sup:new_clock(ClockName, Bpm),
			gen_server:cast(Pid, start),
			{ok, Pid}
	end.

%% 

change_bpm(Bpm) -> change_bpm(default, Bpm). 

change_bpm(Clock, Bpm) when
	  is_atom(Clock) and Bpm > 0 ->
	case 
		check_existing(Clock) of
		{ok, Pid} ->
			change_bpm(Pid, Bpm);
		{error, noclock} -> 
			{error, noclock}
	end;

change_bpm(Clock, Bpm) when
	  is_pid(Clock) and Bpm > 0 ->
	gen_server:call(Clock, {bpm, Bpm}),
	gen_server:cast(Clock, start),
	{ok, Bpm}.

%%

sched(Ref, Func) -> sched(default, Ref, Func).

sched(Clock, Ref, Func)  when 
	  is_atom(Clock) and is_function(Func)->

	case 
		check_existing(Clock) of
		{ok, Pid} ->
			case 
				erlang:fun_info(Func, arity) of
				{arity, 1} -> sched(Pid, Ref, Func);
				_ -> {error, badarity}
			end;

		{error, noclock} -> 
			{error, noclock}
	end;

sched(Clock, Ref, Func)  when is_function(Func) and is_pid(Clock) ->
	case 
		erlang:fun_info(Func, arity) of
		{arity, 1} -> 
			gen_server:cast(Clock, {sched, {Ref, Func}});
		_ ->
			{error, badarity}
	end.


unsched(Ref) -> unsched(default, Ref).

unsched(Clock, Ref)  when 
	  is_atom(Clock) ->
	case 
		check_existing(Clock) of
		{ok, Pid} ->
			unsched(Pid, Ref);
		{error, noclock} -> 
			{error, noclock}
	end;

unsched(Clock, Ref)  when is_pid(Clock) ->
	gen_server:cast(Clock, {unsched, Ref}).



get()-> erlang:apply(?MODULE, get, [default]).

get(Clock) when is_atom(Clock) ->

	case 
		check_existing(Clock) of
		{ok, Pid} ->
			erlang:apply(?MODULE, get, [Pid] );
		{error, noclock} -> 
			{error, noclock}
	end;

get(Clock) when is_pid(Clock)->
	gen_server:call(Clock, get).  




start() ->
	{ok, Pid} = check_existing(default),
	start(Pid).

start(Clock) when
	  is_atom(Clock) ->
	case 
		check_existing(Clock) of
		{ok, Pid} ->
			start(Pid);
		{error, noclock} -> 
			{error, noclock}
	end;

start(Clock) when
	  is_pid(Clock) ->
	gen_server:cast(Clock, start).



stop() ->
	{ok, Pid} = check_existing(default),
	stop(Pid).

stop(Clock) when is_atom(Clock) ->
	case 
		check_existing(Clock) of
		{ok, Pid} ->
			stop(Pid);
		{error, noclock} -> 
			{error, noclock}
	end;

stop(Clock) when is_pid(Clock) ->
	gen_server:call(Clock, stop).



