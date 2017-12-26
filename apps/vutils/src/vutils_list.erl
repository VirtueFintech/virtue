-module (vutils_list).
-author ("Hisham Ismail <mhishami@gmail.com>").

-export ([from_bin/1]).
-export ([from_integer/1]).
-export ([to_bin/1]).

-spec from_bin(binary()) -> list().
from_bin(Bin) when is_binary(Bin) ->
  erlang:binary_to_list(Bin).

-spec from_integer(integer()) -> list().
from_integer(Int) when is_integer(Int) ->
  erlang:integer_to_list(Int).

-spec to_bin(list()) -> binary().
to_bin(List) when is_list(List) ->
  erlang:list_to_binary(List).
