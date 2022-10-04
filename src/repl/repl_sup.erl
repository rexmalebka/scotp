-module(repl_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link()->
	process_flag(trap_exit, true),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	io:format(
	  "~s~n",[
		  color:cyan("starting repl supervisor...")
		 ]
	 ),

        SupFlags = #{strategy => one_for_all,
                     intensity => 0,
                     period => 1},

        ChildSpecs = [
                     ],
        {ok, {SupFlags, ChildSpecs}}.



add_repl({file, Path})->
	supervisor:start_child(?MODULE, #{
					  id=> {file, Path},
					  start=>{ repl_file_mgr, start_link, []}			 
					 }
			      ).


