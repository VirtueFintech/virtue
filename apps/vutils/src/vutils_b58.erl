%% MIT License
%%
%% Copyright (c) 2017 VirtueFintech
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%% 
%% 
-module (vutils_b58).
-author ("Hisham Ismail <mhishami@gmail.com>").

-export ([decode/1]).
-export ([encode/1]).

-spec decode( list() | binary()) -> binary().
decode(List) when is_list(List) ->
  base58:base58_to_binary(List);  
decode(Bin) when is_binary(Bin) ->
  base58:base58_to_binary(binary_to_list(Bin)).

-spec encode(list() | binary()) -> binary().
encode(List) when is_list(List) ->
  list_to_binary(base58:binary_to_base58(list_to_binary(List)));
encode(Bin) when is_binary(Bin) ->
  list_to_binary(base58:binary_to_base58(Bin)).
