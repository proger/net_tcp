-module(msvlq).
-export([decode/1, encode/1, encode_naive/1]).

% Microsoft flavor of Variable-Length Quantity

-spec decode(binary()) -> {integer(), binary()}.

decode(Bin) ->
    {ResBin, T} = decode_acc(Bin, <<>>),
    Pad = byte_pad(bit_size(ResBin)),
    {binary:decode_unsigned(<<0:Pad, ResBin/bits>>, big), T}.

decode_acc(<<0:1, Val:7, T/bits>>, Res) ->
    {<<Val:7, Res/bits>>, T};
decode_acc(<<1:1, Val:7, T/bits>>, Res) ->
    decode_acc(T, <<Val:7 ,Res/bits>>).

-spec encode(integer()) -> binary().

encode(Int) ->
    Bin = binary:encode_unsigned(Int, big),
    encode_acc(Bin, <<>>).

encode_acc(<<>>, Acc) ->
    Acc;
encode_acc(<<0:8, H/binary>>, Acc) ->
    encode_acc(H, Acc);
encode_acc(Bin, Acc) ->
    Size = byte_size(Bin),
    <<P:1, R:7>> = binary:part(Bin, {Size, -1}),
    Rest = binary:part(Bin, {0, Size - 1}),

    Rem = <<0:7, Rest/binary, P:1>>,
    Mark = case binary:decode_unsigned(Rem) of
               0 -> 0;
               _ -> 1
    end,
    encode_acc(Rem, <<Acc/binary, Mark:1, R:7>>).

-spec encode_naive(integer()) -> binary().

encode_naive(Size) when Size =< 16#7f ->
    <<Size>>;
encode_naive(Size) when Size >= 16#80, Size =< 16#3fff ->
    <<((Size band 16#7f) bor 16#80),
      (Size bsr 16#07)>>;
encode_naive(Size) when Size >= 16#4000, Size =< 16#1fffff ->
    <<((Size band 16#7f) bor 16#80),
      (((Size bsr 16#07) band 16#7f) bor 16#80),
      (Size bsr 16#0e)>>;
encode_naive(Size) when Size >= 16#200000, Size =< 16#0fffffff ->
    <<((Size band 16#7f) bor 16#80),
      (((Size bsr 16#07) band 16#7f) bor 16#80),
      (((Size bsr 16#0e) band 16#7f) bor 16#80),
      (Size bsr 16#15)>>;
encode_naive(Size) when Size >= 16#10000000, Size =< 16#0FFFFFFF ->
    <<((Size band 16#7f) bor 16#80),
      (((Size bsr 16#07) band 16#7f) bor 16#80),
      (((Size bsr 16#0e) band 16#7f) bor 16#80),
      (((Size bsr 16#15) band 16#7f) bor 16#80),
      (Size bsr 16#1c)>>.

byte_pad(I) ->
    (8 - (I rem 8)) rem 8.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(VALS, [1, 38, 16#7f, 16#80, 16#3fff, 16#4000, 16#1fffff, 16#200000, 16#0fffffff, 16#1000000, 16#0FFFFFF]).

encode_test_() ->
    [?_assertEqual({I, encode_naive(I)}, {I, msvlq:encode(I)})
     || I <- ?VALS].

decode_test_() ->
    [?_assertEqual({I, <<>>}, msvlq:decode(msvlq:encode(I)))
     || I <- ?VALS].

-endif.
