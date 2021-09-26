%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_auth_http_cli).

-include("emqx_auth_http.hrl").

-export([ request/6
        , feedvar/2
        , feedvar/3
        ]).

%%--------------------------------------------------------------------
%% HTTP Request
%%--------------------------------------------------------------------

request(PoolName, get, Path, Headers, Params, Timeout) ->
    NewPath = Path ++ "?" ++ binary_to_list(cow_qs:qs(bin_kw(Params))),
    do_request(get, PoolName, {NewPath, Headers}, Timeout);

request(PoolName, post, Path, Headers, Params, Timeout) ->
    Body = case proplists:get_value(<<"content-type">>, Headers) of
               <<"application/x-www-form-urlencoded">> ->
                   cow_qs:qs(bin_kw(Params));
               <<"application/json">> ->
                   emqx_json:encode(bin_kw(Params))
           end,
    do_request(post, PoolName, {Path, Headers, Body}, Timeout).

do_request(Method, PoolName, Req, Timeout) ->
    do_request(Method, PoolName, Req, Timeout, 3).

%% Only retry when connection closed by keepalive
do_request(_Method, _PoolName, _Req, _Timeout, 0) ->
    {error, normal};
do_request(Method, PoolName, Req, Timeout, Retry) ->
    case emqx_http_client:request(Method, PoolName, Req, Timeout) of
        {error, normal} ->
            do_request(Method, PoolName, Req, Timeout, Retry - 1);
        {error, {closed, _Reason}} ->
            do_request(Method, PoolName, Req, Timeout, Retry - 1);
        {error, Reason} ->
            {error, Reason};
        {ok, StatusCode, _Headers} ->
            {ok, StatusCode, <<>>};
        {ok, StatusCode, _Headers, Body} ->
            {ok, StatusCode, Body}
    end.

%% TODO: move this conversion to cuttlefish config and schema
bin_kw(KeywordList) when is_list(KeywordList) ->
    [{bin(K), bin(V)} || {K, V} <- KeywordList].

bin(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom));
bin(Int) when is_integer(Int) ->
    integer_to_binary(Int);
bin(Float) when is_float(Float) ->
    float_to_binary(Float, [{decimals, 12}, compact]);
bin(List) when is_list(List)->
    list_to_binary(List);
bin(Binary) when is_binary(Binary) ->
    Binary.

%%--------------------------------------------------------------------
%% Feed Variables
%%--------------------------------------------------------------------

feedvar(Params, ClientInfo = #{clientid := ClientId,
                               protocol := Protocol,
                               peerhost := Peerhost}) ->
    lists:map(fun({Param, "%u"}) -> {Param, maps:get(username, ClientInfo, null)};
                 ({Param, "%c"}) -> {Param, ClientId};
                 ({Param, "%r"}) -> {Param, Protocol};
                 ({Param, "%a"}) -> {Param, inet:ntoa(Peerhost)};
                 ({Param, "%P"}) -> {Param, maps:get(password, ClientInfo, null)};
                 ({Param, "%p"}) -> {Param, maps:get(sockport, ClientInfo, null)};
                 ({Param, "%C"}) -> {Param, maps:get(cn, ClientInfo, null)};
                 ({Param, "%d"}) -> {Param, maps:get(dn, ClientInfo, null)};
                 ({Param, "%A"}) -> {Param, maps:get(access, ClientInfo, null)};
                 ({Param, "%t"}) -> {Param, maps:get(topic, ClientInfo, null)};
                 ({Param, "%m"}) -> {Param, maps:get(mountpoint, ClientInfo, null)};
                 ({Param, Var})  -> {Param, Var}
              end, Params).

feedvar(Params, Var, Val) ->
    lists:map(fun({Param, Var0}) when Var0 == Var ->
                      {Param, Val};
                 ({Param, Var0}) ->
                      {Param, Var0}
              end, Params).

