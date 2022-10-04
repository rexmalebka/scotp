%%%-------------------------------------------------------------------
%% @doc scotp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(scotp_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                  #{
                    id => sc_mgr,
                    start => {sc_mgr, start_link, []}
                   },
                  #{
                    id => clock_sup,
                    start => {clock_sup, start_link, []}
                   },
                  #{
                    id => seq_sup,
                    start => {seq_sup, start_link, []}
                   }                
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
