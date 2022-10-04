%%%-------------------------------------------------------------------
%% @doc scotp public API
%% @end
%%%-------------------------------------------------------------------

-module(scotp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = scotp_sup:start_link(),

    clock:new(default),

    seq:new(default, s1),
    seq:new(default, s2),
    seq:new(default, s3),
    seq:new(default, s4),

    {ok, Pid}.

stop(_State) ->
    ok.

%% internal functions
load_buffers()->
	666.
