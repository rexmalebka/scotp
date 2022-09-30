-module(clock_mgr).
-behaviour(gen_server).

-export([ start_link/2 ]).
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

start_link(ClockName, Bpm) ->
  gen_server:start_link({local, ClockName}, ?MODULE, [Bpm], []).

init([Bpm]) ->
  Timeout = trunc(60*1000/Bpm),
  State = #{
            bpm=>Bpm,
            timeout => Timeout,
            sched=>#{}
           },

  {ok, State}.

handle_call(get, _From, State)->
  {reply, State, State};

handle_call(stop, _From, State=#{ref:=Ref})->  
  timer:cancel(Ref),
  {reply, State, State};

handle_call({bpm, Bpm}, _From, State=#{ref:=Ref})->
  NewState = maps:merge(State,
                        #{
                          bpm=>Bpm,
                          timeout=> trunc(60*1000/Bpm)
                         }
                       ),
  timer:cancel(Ref),

  {reply, Bpm, NewState};

handle_call(_Msg, _From, State)->
  {reply, _Msg, State}.

handle_cast(start, State=#{timeout:=Timeout})->

  Start = erlang:monotonic_time(millisecond),
  {ok, Ref} = timer:send_interval(Timeout, run),
  NewState = maps:merge(State, #{
               count => 0,
               ref => Ref,
               start => Start           
              }),
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

handle_cast(_Msg, State)->
  {noreply, State}.

handle_info(run, State=#{ count:=Count, sched:=Scheds})->
  maps:foreach(fun(Ref, Func)->
                   
                   spawn(fun()-> 
                             case erlang:apply(Func, [State]) of
                               unsched ->
                                 clock:unsched(self(), Ref);
                               _ -> ok
                             end
                         end)
               end, Scheds),

  NewState=maps:merge(State,#{
                              count => Count+1
                             }
                     ),
  
  {noreply, NewState};

handle_info(_Msg, State)->
  {noreply, State}.


