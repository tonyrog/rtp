-ifndef(__RTP_AVP_HRL__).
-define(__RTP_AVP_HRL__, true).

-define(AVP_PCMU,     0).  %% u-law
-define(AVP_1016,     1).
-define(AVP_G721,     2).
-define(AVP_GSM,      3).
-define(AVP_DVI4,     5).
-define(AVP_DVI4_16,  6).
-define(AVP_LPC,      7).
-define(AVP_PCMA,     8).  %% a-law 
-define(AVP_G722,     9).
-define(AVP_L16x2,    10). %% two channels
-define(AVP_L16,      11).
-define(AVP_MPA,      14).
-define(AVP_G728,     15).
-define(AVP_CelB,     25).
-define(AVP_JPEG,     26).
-define(AVP_nv,       28).
-define(AVP_H261,     31).
-define(AVP_MPV,      32).
-define(AVP_MP2T,     33).
-define(AVP_MAX,      33).

-define(AVP_NONE,     0).
-define(AVP_AUDIO,    1).
-define(AVP_VIDEO,    2).

-record(rtp_avp,
	{
	 name  :: string(),
	 media :: ?AVP_NONE | ?AVP_VIDEO | ?AVP_VIDEO | ?AVP_VIDEO+?AVP_VIDEO,
	 rate  :: float(),
	 channels :: integer()
	}).

-define(AVP(Name,Media,Rate,Channels),
	#rtp_avp { name=Name, media=Media, rate=Rate, channels=Channels }).

-endif.

