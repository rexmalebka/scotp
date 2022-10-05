-module(clock_sup).

-behaviour(supervisor).

-export([
         start_link/0,
         init/1
        ]).

start_link() ->
  process_flag(trap_exit, true),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([])->
  io:format(
    "~s~n",[
            color:cyan("starting clock supervisor...")
           ]
   ),

  SupFlags = #{strategy => one_for_all,
               intensity => 0,
               period => 1},

  ChildSpecs = [
               ],
  {ok, {SupFlags, ChildSpecs}}.
