-module(nmf).
%-export([]).
-compile([export_all, {parse_transform, match_reverse_transform}]).
-include("nmf.hrl").

-match_reverse(encoding).
-match_reverse(mode).

% must contain the Rest binding in Format
-define(DATA(Format, Message),
        data(Format, Pid) ->
               Pid ! {record, Message},
               data(Rest, Pid)).
-define(DATAVAR(Format, Message),
        data(Format, Pid) ->
               case (catch(var_string(Rest))) of
                   {'EXIT', _Reason} ->
                       want_more;
		   {Str, Rest1} ->
                       Pid ! {record, Message},
                       data(Rest1, Pid)
               end).

%% Property Records
?DATA(
   <<?REC_VERSION:8, _Maj:8, _Min:8, Rest/binary>>, version);
?DATAVAR(
   <<?REC_VIA:8, Rest/binary>>, {via, Str});
?DATA(
   <<?REC_MODE:8, Mode:8, Rest/binary>>, {mode, mode(Mode)});

?DATA(
   <<?REC_KNOWN_ENCODING:8, Encoding:8, Rest/binary>>, {encoding, encoding(Encoding)});
?DATAVAR(
   <<?REC_EXTENSIBLE_ENCODING:8, Rest/binary>>, {encoding, Str});

?DATAVAR(
   <<?REC_UPGRADE_REQUEST:8, Rest/binary>>, {upgrade_request, Str});
?DATA(
   <<?REC_UPGRADE_RESPONSE:8, Rest/binary>>, upgrade_response);

?DATA(
   <<?REC_PREAMBLE_END:8, Rest/binary>>, preamble_end);
?DATA(
   <<?REC_PREAMBLE_ACK:8, Rest/binary>>, preamble_ack);

?DATA(
   <<?REC_END:8, Rest/binary>>, session_end);

%% Envelope Records
?DATAVAR(
   <<?REC_SIZED_ENVELOPE:8, Rest/binary>>, {envelope, Str});

data(<<?REC_UNSIZED_ENVELOPE:8, Rest/binary>>, Pid) ->
    case (catch(chunked_data(Rest, <<>>))) of
	{'EXIT', _Reason} ->
	    want_more;
	{Envelope, Rest1} ->
	    Pid ! {record, {envelope, Envelope}},
	    data(Rest1, Pid)
    end;

data(<<>>, Pid) ->
    Pid ! {record, empty},
    done;

data(Data, Pid) ->
    Pid ! {record, {unknown, Data}},
    unknown.


chunked_data(<<0, Rest/binary>>, Acc) ->
    {Acc, Rest};
chunked_data(<<Rest/binary>>, Acc) ->
    {Chunk, Rest1} = var_string(Rest),
    chunked_data(Rest1, [Acc, Chunk]).


var_string(<<Rest/binary>>) ->
    {Len, Rest1} = msvlq:decode(Rest),
    <<Str:Len/binary-unit:8, Rest2/binary>> = Rest1,
    {Str, Rest2}.


encoding(0) -> soap11_utf8;
encoding(1) -> soap11_utf16;
encoding(2) -> soap11_unicodele;
encoding(3) -> soap12_utf8;
encoding(4) -> soap12_utf16;
encoding(5) -> soap12_unicodele;
encoding(6) -> soap12_mtom;
encoding(7) -> soap12_nbfs;  % binary
encoding(8) -> soap12_nbfse; % binary with in-band dictionary
encoding(_) -> undefined.

mode(1) -> singleton_unsized;
mode(2) -> duplex;
mode(3) -> simplex;
mode(4) -> singleton_sized;
mode(_) -> undefined.



record(version) ->
    <<?REC_VERSION:8, 1:8, 0:8>>;
record(_) ->
    ok.


preamble(duplex = Mode, Via, Encoding) when is_binary(Via) ->
    M = mode(Mode),
    ViaLen = msvlq:encode(byte_size(Via)),
    Enc = encoding(Encoding),
    <<
      ?REC_VERSION, 1, 0,
      ?REC_MODE, M,
      ?REC_VIA, ViaLen/binary, Via/binary,
      ?REC_KNOWN_ENCODING, Enc,
      ?REC_PREAMBLE_END
    >>.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

preamble_test_() ->
    ?_assertEqual(preamble(duplex, <<"net.tcp://172.16.254.1:8082/HelloWorld">>, soap12_nbfse),
    <<0,1,0,1,2,2,38,110,101,116,46,116,99,112,58,47,47,
      49,55,50,46,49,54,46,50,53,52,46,49,58,56,48,56,50,
      47,72,101,108,108,111,87,111,114,108,100,3,8,12>>).

-endif.
