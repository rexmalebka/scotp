-module(sc).

-behaviour(gen_server).

-export([start_link/0]).
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).
-export([
         load_synthdef/1,
         new_synth/2,
         set_synth/2,
         load_buffer/1
        ]).

start_link()->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  OSCIp = "localhost",
  OSCPort = 57110,

  {ok, _} = osc_client:start(),
  {ok, OSCClient} = osc_client:connect(OSCIp, OSCPort),

  State = #{
            client => OSCClient,
            synths => #{},
            buffers => #{}
           },
  {ok, State}.



handle_call({synthdef, {load, AbsPath}}, _From, State=#{client:=OSCClient}) ->
  Res = osc_client:call_msg(OSCClient, "/d_load", AbsPath),
  {reply, Res, State};

handle_call({synth, get}, _From, State=#{synths:=Synths}) -> {reply, Synths, State};
handle_call({buffer, get}, _From, State=#{buffers:=Buffers}) -> {reply, Buffers, State};


handle_call(_Msg, _From, State) ->
  {reply,_Msg, State}.

handle_cast({synth, 
             {load, Synthdef, SynthId, Args}}, 
            State=#{
                    client:=OSCClient,
                    synths:=Synths}) ->
  osc_client:cast_msg(OSCClient, "/s_new", [Synthdef, SynthId, 1, 0 | Args]),

  NewSynths = maps:put(SynthId, Args, Synths),
  NewState = maps:update(synths, NewSynths, State),

  {noreply, NewState};

handle_cast({synth, {set, SynthId, Args}},
            State=#{
                    client:=OSCClient,
                    synths:=Synths}) -> 

  osc_client:cast_msg(OSCClient, "/n_set", [SynthId | Args]),
  
  NewSynths = maps:put(SynthId, Args, Synths),
  NewState = maps:update(synths, NewSynths, State),

  {noreply, NewState};

handle_cast({buffer, 
             {load, AbsPath, BufferId}},
            State=#{
                    client:=OSCClient,
                    buffers:=Buffers
                   }) ->

  osc_client:cast_msg(OSCClient, "/b_allocRead", [BufferId, AbsPath]),

  NewBuffers = maps:put(BufferId, AbsPath, Buffers),
  NewState = maps:update(buffers, NewBuffers, State),

  {noreply, NewState};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.






gen_id(IdMap)->
  Id = rand:uniform(8000),
  gen_id(Id, IdMap).

gen_id(Id, IdMap) ->
  case maps:is_key(Id, IdMap) of
    true ->
      NewId = rand:uniform(8000),
      gen_id(NewId,IdMap);
    false ->
      Id
  end.

load_synthdef(Path) ->
  AbsPath = filename:absname(Path),
  case gen_server:call(?MODULE, {synthdef, {load, [AbsPath]}}) of
    {message, "/done", _} -> 
      {ok, AbsPath};
    {message, _, _} ->  {error, AbsPath}
  end.

new_synth(Synthdef, ArgsMap ) when is_map(ArgsMap)->
  Synths = gen_server:call(?MODULE, {synth, get}),
  SynthId = gen_id(Synths),

  Args = lists:flatten(
           maps:values(
             maps:map(fun(Key, Value) when is_number(Value)->
                          [{s,atom_to_list(Key)}, Value]
                      end, ArgsMap)
            )
          ),

  gen_server:cast(?MODULE, {synth, {load, Synthdef, SynthId, Args}}),
  {ok, SynthId}.

set_synth(SynthId, ControlMap) when 
    is_integer(SynthId) and is_map(ControlMap) ->

  NewControlMap = case 
                    length(maps:values(ControlMap)) rem 2 of
                    1 ->
                      maps:put('_dummy', 0.0, ControlMap);
                    0 ->
                      ControlMap
                  end,

  Args = lists:flatten(
          maps:values(
            maps:map(fun(Key, Value) when is_number(Value) -> 
                         [{s, atom_to_list(Key)}, float(Value)]
                     end, NewControlMap)
           )
          ),
  gen_server:cast(?MODULE, {synth, {set, SynthId, Args}}),
  {ok, SynthId}.


load_buffer(Path) ->
  AbsPath = filename:absname(Path),

  Buffers = gen_server:call(?MODULE, {buffer, get}),
  BufferId = gen_id(Buffers),

  gen_server:cast(?MODULE, {buffer, {load, {s,AbsPath}, BufferId}}),
  {ok, BufferId}.
