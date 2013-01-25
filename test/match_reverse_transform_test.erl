-module(match_reverse_transform_test).
-compile([{parse_transform, match_reverse_transform}]).
-include_lib("eunit/include/eunit.hrl").

-match_reverse(reverse).
reverse(1) -> ok;
reverse(2) -> fine;
reverse(_) -> unk.

reverse_test_() ->
    ?_assertEqual(reverse(ok), 1).
