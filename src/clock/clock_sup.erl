-module(clock_sup).

-behaviour(supervisor).

-export([
         start_link/0,
         init/1
        ]).

-export([
        list_clocks/0,
        new_clock/2
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

list_clocks() ->
  supervisor:which_children(?MODULE).

new_clock(ClockName, Bpm) ->
  supervisor:start_child(?MODULE,
                         #{
                           id => ClockName,
                           start => { clock_mgr, start_link, [ClockName, Bpm] }
                          }).
stop_clock(ClockPid) ->
  supervisor:terminate_child(?MODULE, ClockPid).
