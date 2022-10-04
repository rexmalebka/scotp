-module(seq_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([
	 list_seqs/0,
	 terminate_seq/1,
	 new_seq/1 
	]).

-define(SERVER, ?MODULE).

start_link() ->
	process_flag(trap_exit, true),
	supervisor:start_link({local, ?MODULE }, ?MODULE, []).

init([]) ->
	io:format(
	  "~s~n",[
		  color:cyan("starting seq supervisor...")
		 ]
	 ),

	sc:load_synthdef("priv/supercollider/scotp_seq.scsyndef"),
	load_default_buffers(),

	SupFlags = #{strategy => one_for_all,
		     intensity => 0,
		     period => 1},

	ChildSpecs = [
		     ],
	{ok, {SupFlags, ChildSpecs}}.

load_default_buffers()->

	io:format(
	  "~s~n",[
		  color:cyan("Loading seq sound bank...")
		 ]
	 ),
	{ok, SoundDirectories} = file:list_dir("priv/soundbank"),
	NewSD = [
		 filename:join(
		   filename:absname("priv/soundbank"),
		   X
		  ) || X <- SoundDirectories ],

	AllFiles_ = lists:filtermap(
		      fun(Path)->
				      case file:list_dir(Path) of
					      {ok, FNames} ->
						      {true,
						       lists:map(
							fun(FName)->
									filename:join([
										       "priv/soundbank",
										       Path,
										       FName

										      ])
							end,						       
							FNames
						       )};
					      {error,_ } -> false
				      end
		      end,
		      NewSD
		     ),
	AllFiles = lists:filtermap(
		     fun(File) ->
				     case filename:extension(File) of
					     ".wav" ->
						     {true, File};
					     _ -> false
				     end
		     end,
		     lists:append(AllFiles_)
		    ),

	lists:foreach(
	  fun(File)->
			  sc:load_buffer(File)
	  end,
	  AllFiles
	 ),
	sc:load_buffer(filename:absname("priv/soundbank/silence.wav")),
	ok.


list_seqs()->
	supervisor:which_children(?MODULE).

new_seq(SeqName) ->
	supervisor:start_child(?MODULE, #{
					  id=>SeqName,
					  start=>{ seq_mgr, start_link, [SeqName] }
					 }).

terminate_seq(Pid) ->
	supervisor:terminate_child(?MODULE, Pid).



handle_info({'EXIT', _From, normal}, State) ->
	error_logger:error_msg("The seq has gracefully stopped."),
	{noreply, State};

handle_info({'EXIT', _From, shutdown}, State) ->
	error_logger:error_msg("The seq has forcely stopped."),
	{noreply, State}.
