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
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	State = #{},
	timer:send_interval(200, eval),
	{ok, State}.


handle_call({add, Path}, _From, State)->
	NewState = maps:put(
		     Path,
		     #{
		       pid => spawn(fun()-> ok end),
		       last => ""
		      },
		     State
		    ),
	{reply, Path, NewState};

handle_call(_Msg, _From, State)->
	{reply, _Msg, State}.



handle_cast(_Msg, State)->
	{noreply, State}.


handle_info(eval, State)->
	NewState = maps:map(fun(Path, FileState) ->
					    case filelib:is_regular(Path) of
						    true ->
							    case file:read_file(Path) of
								    {ok, Data} ->
									    case maps:get(last, FileState) == Data of
										    true ->
											    FileState;
										    false ->
											    exit(maps:get(pid, FileState),kill),
											    Pid = spawn_file(Path),
											    maps:merge(
											      FileState,
											      #{
												last => Data,
												pid => Pid
											       }
											     )
									    end;
								    {error, _ } ->
									    FileState
							    end;
						    false ->
							    FileState
					    end
			    end,
			    State
			   ),

	{noreply, NewState};

handle_info(_Msg, State)->
	{noreply, State}.

spawn_file(Path)->
	spawn(fun()->					       
			      case file:eval(Path) of
				      ok -> ok;
				      {error, Reason} ->
					      io:format("~p~n",[Reason])
			      end
	      end).
