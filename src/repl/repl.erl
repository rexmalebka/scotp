-module(repl).

watch(Path) when is_list(Path) ->
	case filelib:is_regular(Path) of
		true ->
			o;
		false ->
			{error, notsupported}
	end.

