-module(repl_file_mgr).
-behaviour(gen_server).

-export([start_link/0]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2
	]).

start_link()->
	gen_server:start_link({local, SeqName}, ?MODULE, [], []).

init([]) ->
	State = #{},
	{ok, State}.


handle_call({watch, Path}, _From, State)->
	{ok, TRef} = timer:send_interval(200, {repl, {file,  Path}}),
	NewState = maps:put(
		     {file, Path},
		     #{
		       ref => TRef,
		       last => ""
		      },
		     State
		    ),
	{reply, Tref, NewState};

handle_call(_Msg, _From, State)->
	{reply, _Msg, State}.



handle_cast(_Msg, State)->
	{noreply, State}.


handle_info({repl, {file, Path}}, State)->
	FileMap = maps:get({file,Path}, State),
	NewFile = case filelib:is_regular(Path) of
		true ->
			case file:read(Path) of
				{ok, Data} ->
					case maps:get(last, FileMap) == Data of
						true ->
							FileMap;
						false ->
							file:eval(Path),
							maps:put(
							  last,
							  Data,
							  FileMap
							 )
					end;
				{error, _ } ->
					FileMap
			end;	
		false ->
			FileMap
	end,
	NewState = maps:put({file,Path}, NewFile, State),
	{noreply, NewState}.

handle_info(_Msg, State)->
	{noreply, State}.



