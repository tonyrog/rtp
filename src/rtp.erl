%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    RTP/RTCP
%%% @end
%%% Created : 23 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(rtp).

-export([init_sink/2, update_sink/2]).
-export([init_source/3, next/3]).

-export([decode/1, encode/2]).

-export([to_snd/1, from_snd/1]).
-export([profile/0, profile/1]).

-export([test/0]).

-include("../include/rtp.hrl").
-include("../include/rtp_avp.hrl").

%% decode RTP header
decode(Bin) ->
    case Bin of
	<<?RTP(Verion,P,1,CC,M,Pt,Seq,Ts,SSRC,CSRC),
	  XType:16, XLen:16, XData:XLen/binary-unit:32, Data/binary>> ->
	    {#rtp { version=Verion,p=P,x=1,cc=CC,m=M,pt=Pt,seq=Seq,
		    ts=Ts,
		    ssrc=SSRC,
		    csrc=[ SRC || <<SRC:32>> <= CSRC ],
		    ext={XType,XData} },
	     Data};
	<<?RTP(Verion,P,0,CC,M,Pt,Seq,Ts,SSRC,CSRC), Data/binary>> ->
	    {#rtp { version=Verion,p=P,x=0,cc=CC,m=M,pt=Pt,seq=Seq,
		    ts=Ts,
		    ssrc=SSRC,
		    csrc=[ SRC || <<SRC:32>> <= CSRC ],
		    ext=undefined },
	     Data}
    end.

encode(Rtp=#rtp { version=Version,p=P,x=_X,m=M,pt=Pt,seq=Seq,
		  ts=Ts, ssrc=SSRC, csrc=CSRC, ext=Ext
		}, Data) ->
    io:format("Encode: ~p\n", [Rtp]),
    CC = length(CSRC),
    CSRCBin = << <<SRC:32>> || SRC <- CSRC >>,
    case Ext of
	undefined -> 
	    << ?RTP(Version,P,0,CC,M,Pt,Seq,Ts,SSRC,CSRCBin), Data/binary >>;
	{XType,XData} ->
	    << ?RTP(Version,P,1,CC,M,Pt,Seq,Ts,SSRC,CSRCBin),
	       XType:16,((byte_size(XData)+3) div 4):16, XData/binary,
	       Data/binary >>
    end.

%% audio video profile
profile(Pt) ->
    element(Pt+1, profile()).
profile() ->
    {
     ?AVP("PCMU", ?AVP_AUDIO, 8000.0,   1),  %% mu-law
     ?AVP("1016", ?AVP_AUDIO, 8000.0,   1),
     ?AVP("G721", ?AVP_AUDIO, 8000.0,   1),
     ?AVP("GSM",  ?AVP_AUDIO, 8000.0,   1),
     ?AVP("-",    ?AVP_AUDIO,  0.0,     0),
     ?AVP("DVI",  ?AVP_AUDIO, 8000.0,   1),
     ?AVP("DVI",  ?AVP_AUDIO, 16000.0,  1),
     ?AVP("LPC",  ?AVP_AUDIO, 8000.0,   1),
     ?AVP("PCMA", ?AVP_AUDIO, 8000.0,   1),   %% a_law
     ?AVP("G722", ?AVP_AUDIO, 8000.0,   1),
     ?AVP("L16",  ?AVP_AUDIO, 44100.0,  2),
     ?AVP("L16",  ?AVP_AUDIO, 44100.0,  1),
     ?AVP("-",    ?AVP_NONE,  0.0,      0),
     ?AVP("-",    ?AVP_NONE,  0.0,      0),
     ?AVP("MPA",  ?AVP_AUDIO, 90000.0,  0),
     ?AVP("G728", ?AVP_AUDIO, 8000.0,   1),
     ?AVP("-",    ?AVP_AUDIO,  0.0,     0),
     ?AVP("-",    ?AVP_AUDIO,  0.0,     0),
     ?AVP("-",    ?AVP_AUDIO,  0.0,     0),
     ?AVP("-",    ?AVP_AUDIO,  0.0,     0),
     ?AVP("-",    ?AVP_AUDIO,  0.0,     0),
     ?AVP("-",    ?AVP_AUDIO,  0.0,     0),
     ?AVP("-",    ?AVP_AUDIO,  0.0,     0),
     ?AVP("-",    ?AVP_AUDIO,  0.0,     0),
     ?AVP("-",    ?AVP_VIDEO,  0.0,     0),
     ?AVP("CelB", ?AVP_VIDEO,  90000.0, 0),
     ?AVP("JPEG", ?AVP_VIDEO,  90000.0, 0),
     ?AVP("-",    ?AVP_VIDEO,  0.0,     0),
     ?AVP("nv",   ?AVP_VIDEO,  90000.0, 0),
     ?AVP("-",    ?AVP_VIDEO,  0.0,     0),
     ?AVP("-",    ?AVP_VIDEO,  0.0,     0),
     ?AVP("H261", ?AVP_VIDEO,  90000.0, 0),
     ?AVP("MPV",  ?AVP_VIDEO,  90000.0, 0),
     ?AVP("MP2T", ?AVP_VIDEO+?AVP_AUDIO, 90000.0, 0),
     ?AVP("-",    ?AVP_NONE,  0,     0) 
    }.

to_snd(Pt) ->
    case Pt of
	?AVP_PCMA -> #{ format=>a_law, channels=>1 };
	?AVP_PCMU -> #{ format=>mu_law, channels=>1 };
	?AVP_L16 -> #{ format=>s16_le, channels=>1 };
	?AVP_L16x2 -> #{ format=>s16_le, channels=>2 }
    end.

from_snd(Format) ->
    case Format of
	s16_le -> ?AVP_L16;
	a_law -> ?AVP_PCMA;
	mu_law -> ?AVP_PCMA
    end.

-define(RTP_SEQ_MOD, 16#10000).

init_sink(S,Seq) ->
    S#rtp_sink { base_seq = Seq,
		 max_seq = Seq,
		 bad_seq = ?RTP_SEQ_MOD + 1,  %% so seq == bad_seq is false
		 cycles = 0,
		 received = 0,
		 received_prior = 0,
		 expected_prior = 0
	       }.

-define(MAX_DROPOUT,  3000).
-define(MAX_MISORDER, 100).
-define(MIN_SEQUENTIAL, 2).

update_sink(S=#rtp_sink{probation=Probation,received=Received,max_seq=MaxSeq}, Seq) ->
    Udelta = (Seq - S#rtp_sink.max_seq) band (?RTP_SEQ_MOD-1),
    if Probation > 0 ->
	    if Seq =:= MaxSeq+1 ->
		    if Probation =:= 1 ->
			    S1 = init_sink(S, Seq),
			    {true, S1#rtp_sink { received = 1 }};
		       true ->
			    {false,S#rtp_sink { probation = Probation-1, max_seq = Seq }}
		    end;
	       true ->
		    {false, S#rtp_sink { probation = ?MIN_SEQUENTIAL - 1,
					 max_seq = Seq }}
	    end;
       Udelta < ?MAX_DROPOUT ->
	    Cycles = if Seq < S#rtp_sink.max_seq ->
			     S#rtp_sink.cycles + ?RTP_SEQ_MOD;
			true ->
			     S#rtp_sink.cycles
		     end,
	    {true, S#rtp_sink { received = Received+1, cycles=Cycles, max_seq=Seq }};
       Udelta =< ?RTP_SEQ_MOD - ?MAX_MISORDER ->
	    if Seq =:= S#rtp_sink.bad_seq ->
		    S1 = init_sink(S, Seq),
		    {true, S1#rtp_sink { received = 1 }};
	       true ->
		    {false, S#rtp_sink { bad_seq = (Seq+1) band (?RTP_SEQ_MOD-1) }}
	    end;
       true -> %% duplicate or reordered packet
	    {true, S#rtp_sink { received = Received+1 }}
    end.
    
init_source(Pt, SSRC, CSRC) ->
    #rtp_source { ssrc=SSRC, csrc=CSRC, pt=Pt, seq=0, ts=0, sent=0, sent_bytes=0 }.

%% generate "next" packet in sequence
next(S=#rtp_source{ssrc=SSRC,csrc=CSRC,pt=Pt,
		   seq=Seq,ts=Ts,sent=Sent,sent_bytes=SentBytes},
     NFrames, Data)
  when is_binary(Data), is_integer(NFrames) ->
    CC = length(CSRC),
    RtpData = encode(#rtp { m=0, cc=CC, pt=Pt,seq=Seq,ts=Ts,
			ssrc=SSRC, csrc=CSRC }, Data),
    Seq1 = (Seq+1) band (?RTP_SEQ_MOD-1),
    Ts1 = Ts+NFrames,
    {RtpData, S#rtp_source{seq=Seq1,ts=Ts1,
			   sent=Sent+1,sent_bytes=SentBytes+byte_size(Data)}}.

test() ->
    S0 = init_source(11, 1, [2]),
    io:format("S0 = ~p\n", [S0]),
    Samples1 =  << <<I:16/signed-little>> || I <- lists:seq(1,16) >>,
    {Packet1, S1} = next(S0, 16, Samples1),
    io:format("S1 = ~p\n", [S1]),
    Samples2 =  << <<I:16/signed-little>> || I <- lists:seq(1,32) >>,
    {Packet2, S2} = next(S1, 32, Samples2),
    io:format("S2 = ~p\n", [S2]),
    ok.

