%%%-------------------------------------------------------------------
%%% @doc clock API. \n
%%% This is a set of functions to create a clock process.
%%% clock process run on x interval of time and process schedule tasks
%%% @end 
%%%-------------------------------------------------------------------

-module(clock).
-author(rexmalebka).
-export([
	 add/1, add/2
	 , get/1
	 , set_bpm/2
	 , sched/2, sched/3
	 , unsched/2
	 , start/1, stop/1
	 , remove/1
	]).


%% @doc
%% the same as add(ClockName, 120).
%% @end

-spec add(ClockName::atom()) -> {ok,pid()}.

add(ClockName) when is_atom(ClockName) ->
	add(ClockName, 120).


%%--------------------------------------------------------------------
%% @doc
%% creates a new clock process, the name must be unique and used for further operations.
%% @end
%%--------------------------------------------------------------------

-spec add(ClockName::atom(), Bpm::non_neg_integer()) -> {ok,pid()}.

add(ClockName, Bpm) when is_atom(ClockName) and (Bpm > 0) ->
	case supervisor:start_child(
	       clock_sup,
	       #{
		 id=> ClockName
		 , start => { clock_mgr, start_link, [ClockName, Bpm] }
		 , restart => transient
		}) of
		{ok, ClockPid} ->
			gen_server:cast(ClockPid, start),
			{ok, ClockPid};
		{error, {already_started, ClockPid}} -> {ok, ClockPid}
	end.

%%--------------------------------------------------------------------
%% @doc 
%% Get current clock state
%% @end
%%--------------------------------------------------------------------

-spec get(Clock::atom() | pid()) -> clock_mgr:clockState() | {error, not_found}.

get(Clock) when is_atom(Clock) -> 
	case lists:search(fun({ChildName, _,_,_}) when ChildName == Clock ->true;
			     (_) -> false
			  end,
			  supervisor:which_children(clock_sup) 
			 ) of
		{value, {_ClockName, ClockPid, _, _}} ->
			gen_server:call(ClockPid, get);
		false -> {error, not_found}
	end;

get(Clock) when is_pid(Clock) -> 
	case lists:search(fun({_, ChildPid,_,_}) when ChildPid == Clock -> true;
			     (_) -> false
			  end,
			  supervisor:which_children(clock_sup) 
			 ) of
		{value, {_ClockName, ClockPid, _, _}} ->
			gen_server:call(ClockPid, get);
		false -> {error, not_found}
	end.


%%--------------------------------------------------------------------
%% @doc
%% sets bpm for a clock process.
%% @end
%%--------------------------------------------------------------------

-spec set_bpm(Clock::atom() | pid(), Bpm::non_neg_integer()) -> ok | {error, not_found}.

set_bpm(Clock, Bpm) when is_atom(Clock) and (Bpm > 0) ->
	case lists:search(fun({ChildName, _,_,_}) when ChildName == Clock ->true;
			     (_) -> false
			  end,
			  supervisor:which_children(clock_sup) 
			 ) of
		{value, {_ClockName, ClockPid, _, _}} ->
			set_bpm(ClockPid, Bpm),
			ok;
		false -> {error, not_found}
	end;

set_bpm(Clock, Bpm) when is_pid(Clock) and (Bpm > 0) ->
	gen_server:cast(Clock, {set, {bpm, Bpm}}),
	ok.


%%--------------------------------------------------------------------
%% @doc
%% schedule a function in a clock process, this will run each beat of time, name reference is obtained by function name.
%% @end
%%--------------------------------------------------------------------

-spec sched(Clock::atom() | pid()
	    , Func::clock_mgr:clockCallback() ) -> ok | {error, not_found}.

sched(Clock, Func) when is_function(Func,1)->
	{name, FuncName} = erlang:fun_info(Func, name),
	sched(Clock, FuncName, Func).

%%--------------------------------------------------------------------
%% @doc
%% schedule a function in a clock process, this will run each beat of time, name reference necesary for unschedule.
%% @end
%%--------------------------------------------------------------------

-spec sched(Clock::atom() | pid()
	    , FuncName::atom()
	    , Func::clock_mgr:clockCallback() ) -> ok | {error, not_found} .

sched(Clock, FuncName, Func) when is_atom(Clock) and is_function(Func,1)->
	case lists:search(fun({ChildName, _,_,_}) when ChildName == Clock ->true;
			     (_) -> false
			  end,
			  supervisor:which_children(clock_sup) 
			 ) of
		{value, {_ClockName, ClockPid, _, _}} ->
			sched(ClockPid, FuncName, Func),
			ok;
		false -> {error, not_found}
	end;

sched(Clock, FuncName, Func) when is_pid(Clock) and is_function(Func,1)->
	gen_server:cast(Clock, {sched, {FuncName, Func}}),
	ok.


%%--------------------------------------------------------------------
%% @doc
%% unschedule a function in a clock process, you need to schedule it again if you want to use it
%% @end
%%--------------------------------------------------------------------

-spec unsched(Clock::atom() | pid()
	      , FuncName::atom()) -> ok.

unsched(Clock, Func) when is_function(Func,1)->
	{name, FuncName} = erlang:fun_info(Func, name),
	unsched(Clock, FuncName);

unsched(Clock, FuncName) when is_atom(Clock) ->
	case lists:search(fun({ChildName, _,_,_}) when ChildName == Clock ->true;
			     (_) -> false
			  end,
			  supervisor:which_children(clock_sup) 
			 ) of
		{value, {_ClockName, ClockPid, _, _}} ->
			unsched(ClockPid, FuncName),
			ok;
		false -> ok
	end;

unsched(Clock, FuncName) when is_pid(Clock) ->
	gen_server:cast(Clock, {unsched, FuncName}),
	ok.


%%--------------------------------------------------------------------
%% @doc
%% set start status to a clock process, count will continue where it was left.
%% @end
%%--------------------------------------------------------------------

-spec start(Clock::atom()|pid()) -> ok.

start(Clock) when is_atom(Clock) ->
	case lists:search(fun({ChildName, _,_,_}) when ChildName == Clock ->true;
			     (_) -> false
			  end,
			  supervisor:which_children(clock_sup) 
			 ) of
		{value, {_ClockName, ClockPid, _, _}} ->
			start(ClockPid),
			ok;
		false -> ok
	end;

start(Clock) when is_pid(Clock) ->
	gen_server:cast(Clock, start),
	ok.

%%--------------------------------------------------------------------
%% @doc
%% set stop status to a clock process, count will retain its value
%% @end
%%--------------------------------------------------------------------

-spec stop(Clock::atom()|pid()) -> ok.

stop(Clock) when is_atom(Clock) ->
	io:format("asfdsf",[]),
	case lists:search(fun({ChildName, _,_,_}) when ChildName == Clock ->true;
			     (_) -> false
			  end,
			  supervisor:which_children(clock_sup) 
			 ) of
		{value, {_ClockName, ClockPid, _, _}} ->
			stop(ClockPid),
			ok;
		false -> ok
	end;

stop(Clock) when is_pid(Clock) ->
	io:format("stoping",[]),
	gen_server:cast(Clock, stop),
	ok.


%%--------------------------------------------------------------------
%% @doc
%% remove a clock process, stopping all the scheduled functions.
%% @end
%%--------------------------------------------------------------------

-spec remove(Clock::atom()|pid()) -> ok.

remove(Clock) when is_atom(Clock) ->
	case lists:search(fun({ChildName, _,_,_}) when ChildName == Clock ->true;
			     (_) -> false
			  end,
			  supervisor:which_children(clock_sup) 
			 ) of
		{value, {ClockName, ClockPid, _, _}} ->
			gen_server:cast(ClockPid, terminate),
			supervisor:terminate_child(clock_sup, ClockName),
			supervisor:delete_child(clock_sup, ClockName);
		false -> ok
	end;

remove(Clock) when is_pid(Clock) ->
	case lists:search(fun({_, ChildPid,_,_}) when ChildPid == Clock -> true;
			     (_) -> false
			  end,
			  supervisor:which_children(clock_sup) 
			 ) of
		{value, {ClockName, ClockPid, _, _}} ->
			gen_server:cast(ClockPid, terminate),
			supervisor:terminate_child(clock_sup, ClockName),
			supervisor:delete_child(clock_sup, ClockName);
		false -> ok
	end.

