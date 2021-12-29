%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    RTCP
%%% @end
%%% Created : 23 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(rtcp).

-include("../include/rtcp.hrl").

-define(TEST, true).


-export([decode/1, encode/1]).
-ifdef(TEST).
-export([test_sdes/0]).
-endif.


decode(Bin) ->
    case Bin of
	<<?RTCP_COMMON(Version,P,Count,Pt,Length),
	  Bin1:Length/unit:32-binary, _Trail/binary>> ->
	    %% warn if _Trail != <<>>?
	    Common = #rtcp_common { version=Version,p=P,count=Count,
				    pt=Pt, length=Length },
	    case Pt of
		?RTCP_SR ->
		    case Bin1 of
			<<?SR(SSRC, NtpSec, NtpFrac, RtpTs,
			      PSent, OSent, RRBin) >> ->
			    {RRList,_Bin2} = decode_rr_list(RRBin, Count),
			    SR=#sr { ssrc=SSRC, ntp_sec=NtpSec,
				     ntp_frac=NtpFrac,
				     rtp_ts=RtpTs, psent=PSent, osent=OSent,
				     rr=RRList },
			    #rtcp{common=Common, data=SR}
		    end;
		?RTCP_RR ->
		    case Bin1 of
			<<?RR(SSRC, RRBin)>> ->
			    {RRList,_Bin2} = decode_rr_list(RRBin, Count),
			    RR = #rr { ssrc=SSRC, rr=RRList },
			    #rtcp{common=Common, data=RR}
		    end;
		?RTCP_SDES ->
		    case Bin1 of
			<<?SDES(SRC, SDesBin)>> ->
			    {SDesList,_Bin2} = 
				decode_sdes_list(SDesBin,Count),
			    SDES=#sdes { src=SRC, item=SDesList},
			    #rtcp{common=Common, data=SDES}
		    end;
		?RTCP_BYE ->
		    {SrcList, Bin2} = decode_src_list(Bin1, Count),
		    Bye=#bye { src=SrcList, reason=Bin2 },
		    #rtcp{common=Common,data=Bye};
		?RTCP_APP ->
		    App=#app { count=Count, data=Bin1 },
		    #rtcp{common=Common,data=App};
		_ ->
		    io:format("rtcp: tag=~w not handled\n", [Pt]),
		    undefined
	    end;
	_ ->
	    io:format("rtcp: mismatch binary ~p not handled\n", [Bin]),
	    undefined
    end.

decode_rr_list(Bin, N) ->
    decode_rr_list(Bin, N, []).
decode_rr_list(Bin, 0, Acc) ->
    {lists:reverse(Acc), Bin};
decode_rr_list(Bin, I, Acc) ->
    case Bin of
	<< ?RR_ITEM(SSRC, Fraction, Lost, LastSeq,Jittter,Lsr,DLsr), 
	   Bin1/binary >> ->
	    decode_rr_list(Bin1, I-1,
			   [#rr_item { ssrc=SSRC, fraction=Fraction, 
				       lost=Lost, last_seq=LastSeq,
				       jitter=Jittter,
				       lsr=Lsr,dlsr=DLsr } | Acc])
    end.

decode_sdes_list(Bin, N) ->
    decode_sdes_list(Bin, N, []).
decode_sdes_list(Bin, 0, Acc) ->
    {lists:reverse(Acc), Bin};
decode_sdes_list(Bin, I, Acc) ->
    case Bin of
	<<?SDES_ITEM(Type,Length,Data), Bin1/binary>> ->
	    AType=element(Type,{cname,name,email,phone,loc,tool,note,priv}),
	    decode_sdes_list(Bin1, I-1,
			     [#sdes_item{type=AType,data=Data} | Acc])
    end.

decode_src_list(Bin, N) ->
    decode_src_list(Bin, N, []).

decode_src_list(Bin, 0, Acc) ->
    {lists:reverse(Acc), Bin};
decode_src_list(Bin, I, Acc) ->
    case Bin of
	<<SRC:32, Bin1/binary>> ->
	    decode_src_list(Bin1, I-1, [SRC|Acc])
    end.
	    
encode(#rtcp{ common=#rtcp_common { version=Version,p=P}, data=Data}) ->
    {Pt,Count,Bin} = encode_item(Data),
    0 = byte_size(Bin) rem 4,       %% assert
    Length = byte_size(Bin) div 4,  %% in words
    <<?RTCP_COMMON(Version,P,Count,Pt,Length), Bin/binary>>.

encode_item(#sr { ssrc=SSRC, ntp_sec=NtpSec, ntp_frac=NtpFrac,
		  rtp_ts=RtpTs, psent=PSent, osent=OSent,
		  rr=RRList }) ->
    Count = length(RRList),
    RRBin = encode_rr_list(RRList),
    {?RTCP_SR,Count,
     <<?SR(SSRC, NtpSec, NtpFrac, RtpTs, PSent, OSent, RRBin) >>};
encode_item(#rr { ssrc=SSRC, rr=RRList }) ->
    Count = length(RRList),
    RRBin = encode_rr_list(RRList),
    {?RTCP_RR,Count,<<?RR(SSRC, RRBin)>>};
encode_item(#sdes { src=SRC, item=SDesList}) ->
    Count = length(SDesList),
    SDesBin = encode_sdes_list(SDesList),
    %% terminate with zeros=RTCP_SDES_END, pad to 32-bit boundry (1,2,3,4) bytes
    Pad = (4 - (byte_size(SDesBin) band 16#3)), 
    {?RTCP_SDES, Count,	<<?SDES(SRC, SDesBin), 0:Pad/unit:8>>};
encode_item(#bye { src=SrcList, reason=Reason }) ->
    Count = length(SrcList),
    SrcBin = encode_src_list(SrcList),
    {?RTCP_BYE, Count, <<SrcBin/binary, Reason/binary>>};
encode_item(#app { count=Count, data=Bin }) ->
    {?RTCP_APP, Count, Bin }.

encode_rr_list(List) ->
    << << ?RR_ITEM(SSRC, Fraction, Lost, LastSeq,Jittter,Lsr,DLsr) >> ||
	#rr_item { ssrc=SSRC, fraction=Fraction, lost=Lost, 
		   last_seq=LastSeq,jitter=Jittter,lsr=Lsr,dlsr=DLsr }
	    <- List >>.

encode_sdes_list(List) ->
    << <<?SDES_ITEM((atype(Type)),(byte_size(Data)),Data)>>  ||
	#sdes_item{type=Type,data=Data} <- List >>.

encode_src_list(SrcList) ->
    << <<Src:32>> || Src <- SrcList >>.

atype(Type) ->
    maps:get(Type, #{ cname => ?RTCP_SDES_CNAME,
		      name  => ?RTCP_SDES_NAME,
		      email => ?RTCP_SDES_EMAIL,
		      phone => ?RTCP_SDES_PHONE,
		      loc => ?RTCP_SDES_LOC,
		      tool => ?RTCP_SDES_TOOL,
		      note => ?RTCP_SDES_NOTE,
		      priv => ?RTCP_SDES_PRIV }).

-ifdef(TEST).

test_sdes() ->
    RTcp = #rtcp {
	      common = #rtcp_common { },
	      data = #sdes { src = 123,
			     item = 
				 [#sdes_item { type=cname,
					       data=(<<"tony">>)},
                                  #sdes_item { type=name,
					       data=(<<"Tony Rogvall">>) },
                                  #sdes_item { type=email,
					       data=(<<"tony@rogvall.se">>) },
                                  #sdes_item { type=phone,
					       data=(<<"+46702575687">>) },
                                  #sdes_item { type=loc,
					       data=(<<"Stockholm">>) },
                                  #sdes_item { type=tool,
					       data=(<<"MixMesh">>) },
                                  #sdes_item { type=note,
					       data=(<<"This is insane!">>) },
                                  #sdes_item { type=priv,
					       data=(<<"But I like it...">>) }
				 ]
			   }
	      },
    Bin = encode(RTcp),
    decode(Bin).

-endif.
