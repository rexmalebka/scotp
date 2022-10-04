-module(seq).

-export([
	 new/1,
	 new/2,

	 get_bank/0,

	 seq/2,
	 stop/1
	]).

default_sample_props() ->
	#{
	  dur=>1,
	  bufNum=>none,
	  start=>0.0,
	  rate=>1.0,
	  pitch=>1.0,
	  amp=>1.0,
	  room=>0,
	  stut=>0,
	  t_trig=>1
	 }.

check_existing(SeqName)->
	Seqs = maps:from_list(
		 [{Name, Pid} || {Name, Pid, _, _ }<- seq_sup:list_seqs()]
		),
	case
		maps:get(SeqName, Seqs, noseq) of
		noseq ->
			{error, noseq};
		Pid ->
			{ok, Pid}
	end.


new(SeqName) -> 
	new(default, SeqName).

new(Clock, SeqName) when 
	  is_atom(SeqName) ->

	case clock:get(Clock) of
		{error, noclock} ->
			{error, noclock};
		_ ->
			case check_existing(SeqName) of
				{ok, Pid} -> {ok,Pid};
				{error, noseq} -> 
					{ok,Pid} = seq_sup:new_seq(SeqName),
					clock:sched(Clock, {seq, SeqName},
						    fun(#{timeout:=Timeout})->
								    Pid ! { run, Timeout }
						    end),
					{ok,Pid}
			end
	end.


stop(SeqName) ->
	ok.

seq(miau,miau)  ->
	get_bank();

seq(SeqName, SeqList) when is_list(SeqList) and is_atom(SeqName) ->
	case check_existing(SeqName) of
		{ok, Pid} -> 
			seq(Pid, SeqList);
		{error, noseq} -> 
			{error, noseq}
	end;

seq(SeqPid, SeqList) when is_list(SeqList) and is_pid(SeqPid) ->
	NewSeqList = calc_seq(SeqList),
	gen_server:cast(SeqPid, {set, {seqList, NewSeqList}}),
	NewSeqList.


get_bank()->
	RawBank = sc:get_buffers(),
	RawBankList = maps:to_list(RawBank),
	BankList = lists:map(fun({Key, Props =#{path:=Value}})->
				  NameSplit = filename:split(Value),
				  File = lists:nth(length(NameSplit), NameSplit),
				  {Name,_} = lists:split(
					   length(File)-4,
					   File
					  ),
				  { list_to_atom(Name), Props}
		  end,
		  RawBankList
		 ),
	Bank = maps:from_list(BankList),
	Bank.
	

calc_dur(SeqList, Mult) ->

	Length = lists:foldl(
		   fun({_SampleName, #{dur:=Dur}}, Sum) ->
				   Dur + Sum;
		      (_Sample, Sum)->
				   Sum + 1
		   end, 0, SeqList),

	NewSeqList = lists:map(
		       fun({SampleName, SampleProps=#{dur:=Dur}}) when 
					 is_atom(SampleName) ->
				       NewSampleProps = maps:update(
							  dur,
							  (Mult * Dur)/Length,                                       
							  SampleProps
							 ),
				       {SampleName, NewSampleProps};

			  (SampleName) when
					 is_atom(SampleName) ->
				       {SampleName, #{dur=> Mult/Length}};
			  (SamplesTime) when
					 is_list(SamplesTime) ->
				       calc_dur(SamplesTime, Mult / Length)

		       end,
		       SeqList
		      ),
	NewSeqList.

calc_seq(SeqList) ->
	SoundBank = get_bank(),
	SeqListDur = lists:flatten(
		       calc_dur(SeqList, 1)
		      ),

	lists:map(
	  fun({SampleName, Props}) ->
			  NewSample = case 
					      maps:is_key(SampleName, SoundBank) of
					      true ->
						      Sample = maps:get(SampleName, SoundBank),

						      NewProps = maps:merge(
								   default_sample_props(),
								   maps:merge(
								     Props,
								     maps:remove(path, Sample)
								    )
								  ),
						      {SampleName, NewProps};

					      false ->
						      Sample = maps:get(silence, SoundBank),
						      NewProps = maps:merge(
								   default_sample_props(),
								   maps:merge(
								     Props,
								     maps:remove(path, Sample)
								    )
								  ),
						      {silence, NewProps}
				      end,
			  NewSample
	  end,
	  SeqListDur
	 ).
