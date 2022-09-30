-module(seq).

-export([
         new/1,
         new/2
        ]).

new(SeqName) -> ok.


new(Clock, SeqName) when 
    is_pid(Clock) and
    is_atom(SeqName) ->

  Sequencers = seq_sup:list_seq(),
  FilteredSeq = lists:filter(fun({Id, _, _, _})->
                   Id == SeqName
               end,
               Sequencers),

  case length(FilteredSeq) of
    0 -> 
      {ok,Pid} = seq_sup:new_seq(SeqName),
      clock:sched(Clock, {seq, SeqName},
                  fun(_)->                       
                      io:format("uwu",[]),
                      Pid ! run
                  end),
      {ok,Pid};
    _ -> 
      {SeqName, Pid, _, _} = lists:nth(1,FilteredSeq),
      {ok, Pid}
  end.


