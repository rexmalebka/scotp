-module(seq_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([
        list_seq/0,
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

  SupFlags = #{strategy => one_for_all,
               intensity => 0,
               period => 1},

  ChildSpecs = [
               ],
  {ok, {SupFlags, ChildSpecs}}.

list_seq()->
  supervisor:which_children(?MODULE).

new_seq(SeqName) ->
  supervisor:start_child(?MODULE, #{
                                    id=>SeqName,
                                    start=>{ seq_mgr, start_link, [SeqName] }
                                   }).

stop_seq(Pid) ->
  supervisor:terminate_child(?MODULE, Pid).



handle_info({'EXIT', _From, normal}, State) ->
  error_logger:error_msg("The seq has gracefully stopped."),
  {noreply, State};

handle_info({'EXIT', _From, shutdown}, State) ->
  error_logger:error_msg("The seq has forcely stopped."),
  {noreply, State}.
