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

-module(emqx_message_persistence_pgsql_cli).

-behaviour(ecpool_worker).

-include("emqx_message_persistence_pgsql.hrl").

-include_lib("emqx/include/emqx.hrl").

-export([connect/1]).
-export([parse_query/2]).
-export([ equery/2
        , equery/3
        , equery/1
        ]).

%%--------------------------------------------------------------------
%% Avoid SQL Injection: Parse SQL to Parameter Query.
%%--------------------------------------------------------------------

parse_query(_Par, undefined) ->
    undefined;
parse_query(Par, Sql) ->
    case re:run(Sql, "'%[ucCad]'", [global, {capture, all, list}]) of
        {match, Variables} ->
            Params = [Var || [Var] <- Variables],
            {atom_to_list(Par), Params};
        nomatch ->
            {atom_to_list(Par), []}
    end.

pgvar(Sql, Params) ->
    Vars = ["$" ++ integer_to_list(I) || I <- lists:seq(1, length(Params))],
    lists:foldl(fun({Param, Var}, S) ->
            re:replace(S, Param, Var, [{return, list}])
        end, Sql, lists:zip(Params, Vars)).

%%--------------------------------------------------------------------
%% PostgreSQL Connect/Query
%%--------------------------------------------------------------------

connect(Opts) ->
    Host     = proplists:get_value(host, Opts),
    Username = proplists:get_value(username, Opts),
    Password = proplists:get_value(password, Opts),
    {ok, C} = epgsql:connect(Host, Username, Password, conn_opts(Opts)),
    lists:foreach(fun(Par) ->
        Sql0 = application:get_env(?APP, Par, undefined),
        case parse_query(Par, Sql0) of
            undefined -> ok;
            {_, Params} ->
                Sql = pgvar(Sql0, Params),
                epgsql:parse(C, atom_to_list(Par), Sql, [])
        end
    end,  [auth_query, acl_query, super_query]),
    {ok, C}.


conn_opts(Opts) ->
    conn_opts(Opts, []).
conn_opts([], Acc) ->
    Acc;
conn_opts([Opt = {database, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([Opt = {ssl, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([Opt = {port, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([Opt = {timeout, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([Opt = {ssl_opts, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([_Opt|Opts], Acc) ->
    conn_opts(Opts, Acc).

equery(Sql, Params) ->
    ecpool:with_client(?APP, fun(C) -> epgsql:prepared_query(C, Sql, Params) end).

equery(Sql, Params, ClientInfo) ->
    ecpool:with_client(?APP, fun(C) -> epgsql:prepared_query(C, Sql, replvar(Params, ClientInfo)) end).

equery(Sql) ->
    ecpool:with_client(?APP, fun(C) -> epgsql:squery(C, Sql) end).
%%    ecpool:with_client(?APP, fun(C) -> 
%%        Ref = epgsql:squery(C, Sql),
%%        receive
%%            {C, Ref, {columns, Columns}} ->
%%                %% columns description
%%                Columns;
%%            {C, Ref, {data, Row}} ->
%%                %% single data row
%%                Row;
%%            {C, Ref, {error, _E} = Error} ->
%%                Error;
%%            {C, Ref, {complete, {_Type, Count}}} ->
%%                %% execution of one insert/update/delete has finished
%%                {ok, Count}; % affected rows count
%%            {C, Ref, {complete, _Type}} ->
%%                %% execution of one select has finished
%%                ok;
%%            {C, Ref, done} ->
%%                %% execution of all queries from Sql has been finished
%%                done
%%        end
%%        
%%    end).

replvar(Params, ClientInfo) ->
    replvar(Params, ClientInfo, []).

replvar([], _ClientInfo, Acc) ->
    lists:reverse(Acc);

replvar(["'%u'" | Params], ClientInfo = #{username := Username}, Acc) ->
    replvar(Params, ClientInfo, [Username | Acc]);
replvar(["'%c'" | Params], ClientInfo = #{clientid := ClientId}, Acc) ->
    replvar(Params, ClientInfo, [ClientId | Acc]);
replvar(["'%a'" | Params], ClientInfo = #{peername := {IpAddr, _}}, Acc) ->
    replvar(Params, ClientInfo, [inet_parse:ntoa(IpAddr) | Acc]);
replvar(["'%C'" | Params], ClientInfo, Acc) ->
    replvar(Params, ClientInfo, [safe_get(cn, ClientInfo)| Acc]);
replvar(["'%d'" | Params], ClientInfo, Acc) ->
    replvar(Params, ClientInfo, [safe_get(dn, ClientInfo)| Acc]);
replvar([Param | Params], ClientInfo, Acc) ->
    replvar(Params, ClientInfo, [Param | Acc]).

safe_get(K, ClientInfo) ->
    bin(maps:get(K, ClientInfo, undefined)).

bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(B) when is_binary(B) -> B;
bin(X) -> X.

