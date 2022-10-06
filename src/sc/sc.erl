%%%-------------------------------------------------------------------
%%% @doc
%%% 
%%% @end
%%%-------------------------------------------------------------------

-module(sc).

-author(rexmalebka).

-export([
         load_synthdef/1,

         new_synth/2,
         set_synth/2,

	 get_buffers/0,
         load_buffer/1
        ]).



%%--------------------------------------------------------------------
%% @doc
%% add a synthdef https://doc.sccode.org/Reference/Server-Command-Reference.html#Synth%20Definition%20Commands  
%% @end
%%--------------------------------------------------------------------

-spec add_synthdef(Path::list | iodata()) -> ok.

add_synthdef(Path) when is_list-> 
	AbsPath = filename:absname(Path),
	case filelib:is_dir(AbsPath ) of
		true ->
			% read all the directory
			
			ok;
		false ->
	end.



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
  case gen_server:call(sc_mgr, {synthdef, {load, [AbsPath]}}) of
    {message, "/done", _} -> 
      {ok, AbsPath};
    {message, _, _} ->  {error, AbsPath}
  end.

new_synth(Synthdef, ArgsMap ) when is_map(ArgsMap)->
  Synths = gen_server:call(sc_mgr, {synth, get}),
  SynthId = gen_id(Synths),

  Args = lists:flatten(
           maps:values(
             maps:map(fun(Key, Value) when is_number(Value)->
                          [{s,atom_to_list(Key)}, Value]
                      end, ArgsMap)
            )
          ),

  gen_server:cast(sc_mgr, {synth, {load, Synthdef, SynthId, Args}}),
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
  gen_server:cast(sc_mgr, {synth, {set, SynthId, Args}}),
  {ok, SynthId}.


gen_bufferid(BufferId)->
	#{
	  numChannels:= NumChannels,
	  numFrames := NumFrames
	 } = gen_server:call(sc_mgr, {buffer, {query, BufferId}}),
	case (NumChannels == 0) and ( NumFrames  == 0) of
		true ->
			BufferId;
		false ->
			gen_bufferid(BufferId+1)
	end.

get_buffers()->
	gen_server:call(sc_mgr, {buffer, get}).

load_buffer(Path) ->
  AbsPath = filename:absname(Path),

  Buffers = gen_server:call(sc_mgr, {buffer, get}),
  MaxBuffer = lists:max([0 | maps:keys(Buffers)]),
  BufferId = gen_bufferid(MaxBuffer),

  gen_server:call(sc_mgr, {buffer, {load, AbsPath, BufferId}}),
  {ok, BufferId}.



