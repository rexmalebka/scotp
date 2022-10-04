-module(seq_mgr).
-behaviour(gen_server).

-export([start_link/1]).
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 run/2
	]).

start_link(SeqName) ->
	gen_server:start_link({local, SeqName}, ?MODULE, [], []).

init([]) ->
	Bank = seq:get_bank(),
	#{bufNum:=SilenceId} = maps:get(silence, Bank),
	{ok, SynthId} = sc:new_synth("scotp_seq",#{bufNum=>SilenceId,amp=>1}),
	%SynthId = 666,
	State = #{
		  seqList => [],
		  synthId => SynthId,
		  refs => [],
		  stopped => false
		 },
	{ok, State}.

handle_call(get, _From, State) ->
	{reply, State, State};

handle_call(terminate, _From, State) ->
	{stop, normal, shutdown_ok, State}.


handle_cast(stop, State) ->
	NewState = maps:update(stopped, true, State),
	{noreply, NewState};

handle_cast({set, {seqList, SeqList}}, State) ->
	NewState = maps:update(seqList, SeqList, State),
	{noreply, NewState};

handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info({run, Timeout}, State=#{seqList:=SeqList, synthId := SynthId}) ->
	NewRefs = sched(SeqList, Timeout, 0, [],SynthId),
	NewState = maps:put(refs,  NewRefs, State),
	{noreply, NewState};

handle_info(_Msg, State) ->
	{noreply, State}.


sched([{Sample,Props=#{dur:=Dur}} |SeqList], Timeout, Acc, List, SynthId) ->
	Time = trunc(Timeout * Dur),
	{ok, Tref} = timer:apply_after(Time + Acc,
				       ?MODULE,
				       run,
				       [Props, SynthId]
				      ),
	sched(SeqList, Timeout, Time, [Tref | List], SynthId);

sched([], _Timeout, _Acc, List, _SynthId) ->
	List.

run(Props, SynthId)->
	sc:set_synth(SynthId, Props).
