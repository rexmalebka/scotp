-module(seq_mgr).
-behaviour(gen_server).

-export([start_link/1]).
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-export_type([
              sequencer/0
             ]).

-type sample() :: #{
                    bufferId := integer(),
                    name := atom(),
                    dur := float(),
                    pitch := float(),
                    rate := float(),
                    atk := float(),
                    rel := float(),
                    room := float(),
                    start := float()
                   }.

-type seqList() :: {
        seqLoop : pid(),
        [
         { sampleName : atom(), sample : sample()}
        ]
       }.

-type propList() :: {
        propLoop : pid(),
        [
         { propValue : float() , dur : float()}
        ]
       }.

-type  propMod() :: #{
                      dur := propList(),
                      pitch := propList(),
                      rate := propList(),
                      atk := propList(),
                      rel := propList(),
                      room := propList(),
                      start := propList()
                     }.

-type sequencer() :: #{
                       seqList := seqList(),
                       propMod := propMod(),
                       synthId := integer()
                      }.

-spec init([]) -> {ok, state : sequencer()}. 





start_link(SeqName) ->
  gen_server:start_link({local, SeqName}, ?MODULE, [], []).

init([]) ->
  {ok, SynthId} = sc:new_synth("miau",#{amp=>0}),
  State = #{
            seqList => [],
            synthId => SynthId
           },
  {ok, State}.

handle_call(stop, _From, State) ->
  {stop, normal, shutdown_ok, State}.

handle_cast({set, {seqList, SeqList}}, State) ->
  NewState = maps:update(seqList, SeqList),
  {noreply, NewState};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(run, State) ->
  io:format("run",[]),
  {noreply, State};

handle_info(_Msg, State) ->
  {noreply, State}.


set_seq(SeqList) when is_list(SeqList) ->
  NewSeqList = calc_seq(SeqList),
  gen_server:cast(?MODULE, {set, {seqList, NewSeqList}}),
  ok.

stop()->
  gen_server:call(?MODULE, stop).



calc_dur(SeqList, Mult) ->

  Length = lists:foldl(
             fun({_SampleName, #{dur:=Dur}}, Sum) ->
                 Dur + Sum;
                (_Sample, Sum)->
                 Sum + 1
             end),

  NewSeqList = lists:map(
                 fun({SampleName, SampleProps=#{dur:=Dur}}) when 
                       is_atom(SampleName) ->
                     NewSampleProps = maps:update(
                                        dur,
                                        Mult * Dur,                                       
                                        SampleProps
                                       ),
                     {SampleName, NewSampleProps};

                    (SamplesTime) when
                       is_list(SamplesTime) ->
                     calc_dur(SamplesTime, Mult / Length)

                 end,
                 SeqList
                ),
  NewSeqList.

calc_seq(SeqList) ->
  SoundBank = mseq:get_bank(),

  SeqListDur = lists:flatten(
                 calc_dur(SeqList, 1)
                ),

  ProcessedSeqList = lists:map(
                       fun({SampleName, Props}) ->
                           NewSample = case 
                                        maps:is_key(SampleName, SoundBank) of
                                        true ->
                                          NewProps = maps:merge(
                                                       #{
                                                         pitch => 1,
                                                         rate => 1,
                                                         atk => 0,
                                                         rel => 0,
                                                         room => 0,
                                                         start => 0
                                                        },
                                                       Props
                                                      ),
                                          {SampleName, NewProps};

                                        false ->
                                          {'', #{}}
                                      end,
                           NewSample
                       end,
                       SeqListDur
                      ),

  ProcessedSeqList.







