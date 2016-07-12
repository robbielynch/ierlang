-module(ierl_hmac).

-export([encode/2]).

encode(Parts, Key) ->
  Ctx = lists:foldl(
    fun (Element, Accumulator) ->
      crypto:hmac_update(Accumulator, Element)
    end,
    crypto:hmac_init(sha256, Key),
    Parts
  ),

  hexlify(crypto:hmac_final(Ctx)).

hexlify(Bin) when is_binary(Bin) ->
  << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C)             -> $a + C - 10.
