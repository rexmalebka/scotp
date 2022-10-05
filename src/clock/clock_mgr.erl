%%%-------------------------------------------------------------------
%%% @doc Clock Manager \n
%%% This is a gen_server module that gets scheduled into an existing clock
%%% @end
%%%-------------------------------------------------------------------

-module(clock_mgr).

-author(rexmalebka).

-behaviour(gen_server).

-export([ start_link/2 ]).

-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2
	]).

-export_type([
	      clockCallback/0
	      ,clockState/0
	     ]).

-type clockCallbackArgument() :: #{
				   stopped => true | false | initial
				   , bpm => non_neg_integer()
				   , timeout => non_neg_integer()
				  }.

-type clockCallback()::fun( (clockCallbackArgument()) -> unsched | any()).

-type schedState() :: #{
			atom() => clockCallback()
		       }.

-type clockState() :: #{
			stopped => true | false | initial
			, bpm => non_neg_integer()
			, timeout => non_neg_integer()
			, sched => schedState()
		       }.

start_link(ClockName, Bpm) ->
	gen_server:start_link({local, ClockName}, ?MODULE, [Bpm], []).

init([Bpm]) ->
	Timeout = trunc(60*1000/Bpm),
	State = #{
		  stopped => initial
		  , bpm=>Bpm
		  , timeout => Timeout
		  , sched=>#{}
		 },
	{ok, State}.

%%%-------------------------------------------------------------------
%% API Exports
%%%-------------------------------------------------------------------


%% Call handlers, mainly to get the whole state

handle_call(get, _From, State)-> {reply, State, State};
handle_call(_Msg, _From, State)-> {reply, unknown, State}.


%% Cast handlers

%% Initial start.

handle_cast(start, State=#{timeout:=Timeout, stopped:=initial})->

	Start = erlang:monotonic_time(millisecond),
	{ok, Ref} = timer:send_interval(Timeout, run),
	NewState = maps:merge(State, #{
				       count => 0
				       , ref => Ref
				       , start => Start           
				       , stopped => false
				      }),
	{noreply, NewState};

%% start after a pause.

handle_cast(start, State=#{stopped:=true})->

	Start = erlang:monotonic_time(millisecond),
	NewState = maps:merge(State, #{
				       stopped => false,
				       start => Start           
				      }),
	{noreply, NewState};

%% This justs pause the seq

handle_cast(stop, State=#{stopped:=false})->
	NewState = maps:put(stopped, true, State),
	{noreply, NewState};

%% set functions

handle_cast({set, {bpm, Bpm}}, State=#{ref:=Ref})->
	NewState = maps:merge(State,
			      #{
				bpm=>Bpm,
				timeout=> trunc(60*1000/Bpm)
			       }
			     ),
	timer:cancel(Ref),
	gen_server:cast(self(), start),

	{noreply, NewState};

%% Schedule / Unschedule cast

handle_cast({sched, {Ref, Fun}}, State=#{sched:=Sched})->

	NewSched = maps:put(
		     Ref,
		     Fun,
		     Sched
		    ),

	NewState = maps:put(
		     sched,
		     NewSched,
		     State
		    ),

	{noreply, NewState};

handle_cast({unsched, Ref}, State=#{sched:=Sched})->
	NewSched = maps:remove(
		     Ref,
		     Sched
		    ),

	NewState = maps:put(
		     sched,
		     NewSched,
		     State
		    ),

	{noreply, NewState};

%% terminate the whole process, cleanup function

handle_cast(terminate, State=#{ref:=Ref})->  
	timer:cancel(Ref),
	{stop, normal, State};

handle_cast(_Msg, State)->
	{noreply, State}.



handle_info(run, State=#{ count:=Count, sched:=Scheds, stopped:=false})->
	maps:foreach(fun(Ref, Func)->
				     SeqParent = self(),
				     run_callback(State, Ref, Func, SeqParent )
		     end, Scheds),

	NewState=maps:merge(State,#{
				    count => Count+1
				   }
			   ),

	{noreply, NewState};

handle_info(_Msg, State)->
	{noreply, State}.


%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

run_callback(State, Ref, Func, SeqParent ) ->
	NewState = maps:remove(sched, State),
	spawn(
	  fun() ->
			  case erlang:apply(Func, [NewState]) of
				  unsched ->
					  gen_server:cast(SeqParent, {unsched, Ref});
				  _ -> 
					  ok
			  end
	  end).
