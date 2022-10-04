-module(sc_mgr).

-behaviour(gen_server).

-export([start_link/0]).
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
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

handle_call({buffer, {query, BufferId}}, _From, State=#{client:=OSCClient}) -> 
	{message, "/b_info", [BufferId, NumFrames, NumChannels,SampleRate]} = osc_client:call_msg(OSCClient, "/b_query", [{i,BufferId}]),
	Buffer = #{
		   bufferId => BufferId,
		   numChannels => NumChannels,
		   numFrames => NumFrames, 
		   sampleRate =>SampleRate		  
		  },
	{reply, Buffer, State};

handle_call({buffer, 
             {load, AbsPath, BufferId}},
	    _From,
            State=#{
                    client:=OSCClient,
                    buffers:=Buffers
                   }) ->

  Res = osc_client:call_msg(OSCClient, "/b_allocRead", [BufferId, {s,AbsPath},0,-1]),
  {message, _, [_, NumFrames, NumChannels, SampleRate]} = osc_client:call_msg(OSCClient, "/b_query", [BufferId]),
  SampleInfo =#{
		bufNum =>BufferId,
		path=> AbsPath,
		numFrames=> NumFrames,
		numChannels=> NumChannels,
		samplerate => SampleRate	       
	       },  

  NewBuffers = maps:put(BufferId, SampleInfo, Buffers),
  NewState = maps:update(buffers, NewBuffers, State),

  {reply,Res, NewState};


handle_call(_Msg, _From, State) ->
  {reply,_Msg, State}.

handle_cast({synth, 
             {load, Synthdef, SynthId, Args}}, 
            State=#{
                    client:=OSCClient,
                    synths:=Synths}) ->
	%/s_new
  osc_client:cast_msg(OSCClient, "/s_new", [Synthdef, SynthId, 0, 1 | Args]),

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

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

