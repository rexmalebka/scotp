-module(repl).
-export([
	watch/1
	]).

watch(Path) when is_list(Path) ->
	case filelib:is_regular(Path) of
		true ->
			gen_server:call(repl_file_mgr, {add, Path}),
			{ok, Path};
		false ->
			{error, notregularfile}
	end.


