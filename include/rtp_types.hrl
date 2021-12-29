-ifndef(__RTP_TYPES_HRL__).
-define(__RTP_TYPES_HRL__, true).

-define(RTP_VERSION, 2).

-type uint1() :: 0..1.
-type uint2() :: 0..3.
-type uint4() :: 0..15.
-type uint5() :: 0..31.
-type uint7() :: 0..16#7f.
-type uint8() :: 0..16#ff.
-type uint16() :: 0..16#ffff.
-type uint24() :: 0..16#ffffff.
-type uint32() :: 0..16#ffffffff.

-endif.
