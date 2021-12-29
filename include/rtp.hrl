-ifndef(__RTP_HRL__).
-define(__RTP_HRL__, true).

-include("rtp_types.hrl").

-record(rtp_sink,
	{
	 ssrc,           %% SSRC or CSRC
	 max_seq,
	 cycles,
	 base_seq,
	 bad_seq,
	 probation = 0,
	 expected_prior,
	 received_prior,
	 transmit,
	 jitter,
	 received = 0
	}).

-record(rtp_source,
	{
	 ssrc,       %% synchronization source
	 csrc=[],    %% contributing source (max=15)
	 pt,         %% payload type
	 seq,        %% sequence number
	 ts,         %% timestamp 
	 sent,       %% number of packets sent
	 sent_bytes  %% number of bytes sent
	}).

-record(rtp, 
	{
	 version = ?RTP_VERSION :: ?RTP_VERSION,  %% protocol version
	 p = 0 :: uint1(),          %% padding
	 x    :: uint1(),           %% header extension flag
	 cc   :: uint4(),           %% CSRC count (cobtributing source)
	 m    :: uint1(),           %% marker bit
	 pt   :: uint7(),           %% payload type
	 seq  :: uint16(),          %% sequence number
	 ts   :: uint32(),          %% timestamp
	 ssrc :: uint32(),          %% synchronization source
	 csrc :: [uint32()] | binary(), %% optional CSRC list [cc]
	 ext  :: undefined | {uint16(), binary()}
	}).

-define(RTP(
	  Version,
	  P,
	  X,
	  CC,
	  M,
	  Pt,
	  Seq,
	  Ts,
	  SSRC,
	  CSRC
	 ),
	Version:2, P:1, X:1, CC:4, M:1,Pt:7, Seq:16,
	Ts:32, SSRC:32, CSRC:(CC)/binary-unit:32).

-endif.
