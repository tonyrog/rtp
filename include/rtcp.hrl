-ifndef(__RTCP_HRL__).
-define(__RTCP_HRL__, true).

-include("rtp_types.hrl").

-define(RTCP_SR,   200).
-define(RTCP_RR,   201).
-define(RTCP_SDES, 202).
-define(RTCP_BYE,  203).
-define(RTCP_APP,  204).

-define(RTCP_SDES_END,   0).
-define(RTCP_SDES_CNAME, 1).
-define(RTCP_SDES_NAME,  2).
-define(RTCP_SDES_EMAIL, 3).
-define(RTCP_SDES_PHONE, 4).
-define(RTCP_SDES_LOC,   5).
-define(RTCP_SDES_TOOL,  6).
-define(RTCP_SDES_NOTE,  7).
-define(RTCP_SDES_PRIV,  8).

%% 1x32 bit
-record(rtcp_common,
	{
	version = ?RTP_VERSION :: uint2(), %% protocol version
	p = 0 :: uint1(),      %% padding flag
        count   :: uint5(),      %% varies by packet type
	pt      :: uint8(),      %% RTCP packet type
	length  :: uint16()      %% pkt len in words, w/o this word
	}).

%% 32-bit
-define(RTCP_COMMON(Version,P,Count,Pt,Length),
	Version:2, P:1, Count:5, Pt:8, Length:16).

-define(RTCP_VALID_MASK,  (16#c000 bor 16#2000 bor 16#fe)).
-define(RTCP_VALID_VALUE, ((?RTP_VERSION bsl 14) bor ?RTCP_SR)).

%% 6x32 bit
-record(rr_item,
	{
	 ssrc::uint32(),          %% data source being reported 
	 fraction::uint8(),       %% fraction lost since last SR/RR 
	 lost::uint24(),          %% cumul. no. pkts lost (signed!) 
	 last_seq::uint32(),      %% extended last seq. no. received 
	 jitter::uint32(),        %% interarrival jitter 
	 lsr::uint32(),           %% last SR packet from this source 
	 dlsr::uint32()           %% delay since last SR packet 
	}).

-define(RR_ITEM(SSRC, Fraction, Lost, LastSeq,Jittter,Lsr,DLsr),
	SSRC:32, Fraction:8, Lost:24, LastSeq:32,Jittter:32,Lsr:32,DLsr:32).

%% byte boundry (need pad after last item)
-record(sdes_item,
	{
	 type::cname|name|email|phone|loc|tool|note|priv,
	 length::uint8(),          %% length of item (in octets) 
	 data::binary()            %% text, not null-terminated
	}).

-define(SDES_ITEM(Type,Len,Data),
	Type:8,Len:8,Data:Len/binary).

%% 6x32 + countx6x32 
-record(sr,
	{
	 ssrc::uint32(),     %% sender generating this report 
         ntp_sec::uint32(),  %% NTP timestamp 
         ntp_frac::uint32(),
         rtp_ts::uint32(),   %% RTP timestamp 
         psent::uint32(),    %% packets sent 
         osent::uint32(),    %% octets sent  
	 rr :: [#rr_item{}]  %% variable-length [count] list 
	}).

-define(SR(SSRC, NtpSec, NtpFrac, RtpTs, PSent, OSent, RR),
	SSRC:32, NtpSec:32, NtpFrac:32, RtpTs:32, PSent:32, OSent:32,
	RR/binary).

%% reception report (RR) 
%% 1x32 + countx6x32
-record(rr,
	{
	 ssrc :: uint32(),     %% receiver generating this report 
	 rr :: [#rr_item{}]    %% variable-length [count] list 
	}).

-define(RR(SSRC, RR),
	SSRC:32, RR/binary).

%% 1x32 + countx(bytes) + pad
-record(sdes,
	{
	 src::uint32(),        %% first SSRC/CSRC 
	 item::[#sdes_item{}]  %% list [count] of SDES items 
        }).

-define(SDES(SRC, Item),
	SRC:32, Item/binary).

-record(bye,
	{
	 src::[uint32()],   %% list of [count] sources
	 reason::binary()   %% reason for leaving (rest of packet)
	}).

-define(BYE(SRCList),
	SrcList/binary).

-record(app,
	{
	 count :: uint5(),
	 data :: binary()
	}).

-record(rtcp,
	{
	 common :: #rtcp_common{},
	 data :: #sr{} | #rr{} | #sdes{} | #bye{} | #app{}
	}).

-endif.
