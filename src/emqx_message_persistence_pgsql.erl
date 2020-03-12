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

-module(emqx_message_persistence_pgsql).

-include("emqx_message_persistence_pgsql.hrl").

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-export([ register_metrics/0
        , check/3
        , description/0
        ]).
-export([on_message_publish/2]).

-export([load/1, unload/0]).

%% Called when the plugin application start

-spec(register_metrics() -> ok).
register_metrics() ->
    lists:foreach(fun emqx_metrics:new/1, ?AUTH_METRICS).


%% Called when the plugin application start
load(Env) ->
    io:format("load start "),
    emqx:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]).

%%--------------------------------------------------------------------
%% Auth Module Callbacks
%%--------------------------------------------------------------------

check(ClientInfo = #{password := Password}, AuthResult,
      #{auth_query  := {AuthSql, AuthParams},
        super_query := SuperQuery,
        hash_type   := HashType}) ->
    CheckPass = case emqx_message_persistence_pgsql_cli:equery(AuthSql, AuthParams, ClientInfo) of
                    {ok, _, [Record]} ->
                        check_pass(erlang:append_element(Record, Password), HashType);
                    {ok, _, []} ->
                        {error, not_found};
                    {error, Reason} ->
                        ?LOG(error, "[Postgres] query '~p' failed: ~p", [AuthSql, Reason]),
                        {error, not_found}
                end,
    case CheckPass of
        ok ->
            emqx_metrics:inc(?AUTH_METRICS(success)),
            {stop, AuthResult#{is_superuser => is_superuser(SuperQuery, ClientInfo),
                                anonymous => false,
                                auth_result => success}};
        {error, not_found} ->
            emqx_metrics:inc(?AUTH_METRICS(ignore)), ok;
        {error, ResultCode} ->
            ?LOG(error, "[Postgres] Auth from pgsql failed: ~p", [ResultCode]),
            emqx_metrics:inc(?AUTH_METRICS(failure)),
            {stop, AuthResult#{auth_result => ResultCode, anonymous => false}}
    end.

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | {string(), list()}, emqx_types:client()) -> boolean()).
is_superuser(undefined, _Client) ->
    false;
is_superuser({SuperSql, Params}, ClientInfo) ->
    case emqx_message_persistence_pgsql_cli:equery(SuperSql, Params, ClientInfo) of
        {ok, [_Super], [{true}]} ->
            true;
        {ok, [_Super], [_False]} ->
            false;
        {ok, [_Super], []} ->
            false;
        {error, _Error} ->
            false
    end.

check_pass(Password, HashType) ->
    case emqx_passwd:check_pass(Password, HashType) of
        ok -> ok;
        {error, _Reason} -> {error, not_authorized}
    end.


%% Transform message and return
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};

on_message_publish(Message, _Env) ->
    io:format("Publish ~s~n", [emqx_message:format(Message)]),
%%    Id = case is_binary(Message#message.id) of
%%        true -> binary:list_to_bin(integer_to_list(binary:decode_unsigned(Message#message.id)));
%%        false -> <<"">>
%%    end,
    Topics = string:split(Message#message.topic, "/", all),
    Topicsfree = lists:delete("",Topics),
    Topicsfreeall = lists:delete(<<>>,Topicsfree),
%%    MessageMap = #{
%%    <<"id">> => Id,
    Qos = integer_to_binary(Message#message.qos),
    From = Message#message.from,
    Flags = Message#message.flags,
%%  Headers = Message#message.headers,
    Topic = Message#message.topic,
    T1 = check_if_exist(1,Topicsfreeall),
    T2 = check_if_exist(2,Topicsfreeall),
    T3 = check_if_exist(3,Topicsfreeall),
    T4 = check_if_exist(4,Topicsfreeall),
    T5 = check_if_exist(5,Topicsfreeall),
    T6 = check_if_exist(6,Topicsfreeall),
    T7 = check_if_exist(7,Topicsfreeall),
    T8 = check_if_exist(8,Topicsfreeall),
    T9 = check_if_exist(9,Topicsfreeall),
    Payload = Message#message.payload,
%%    <<"timestamp">> => integer_to_binary(calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(Message#message.timestamp))),
    Ts = Message#message.timestamp,   
    io:format("Payload ~s~n", [Payload]),
    io:format("Topic ~s~n", [Topic]),
    io:format("From ~s~n", [From]),
    io:format("Qos ~s~n", [Qos]),     
%%    Status = "publish",
%%    if 
%%        is_number(Payload) -> { Val, rest } = string:to_float(Payload);
%%        true -> Val = "NULL"
%%    end,
    Sql = "INSERT INTO messages (t1,t2,t3,t4,t5,t6,t7,t8,t9, ts, payload,topic,da,qos) VALUES (", %% $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15)",
    Query = Sql ++ "'" ++ T1
                ++ "','" ++ T2
                ++ "','" ++ T3
                ++ "','" ++ T4
                ++ "','" ++ T5
                ++ "','" ++ T6
                ++ "','" ++ T7
                ++ "','" ++ T8
                ++ "','" ++ T9
                ++ "','" ++ Ts
                ++ "','" ++ binary:bin_to_list(Payload)
                ++ "','" ++ binary:bin_to_list(Topic)
                ++ "','" ++ binary:bin_to_list(From)
                ++ "'," ++ binary:bin_to_list(Qos)
                ++ ")",

%%    Sql = string:concat(Sql,"'"),
%%    Sql = string:concat(Sql,T1),
%%    Sql = string:concat(Sql,"'"),
%%    Sql = Sql ++ "'" ++ T2 ++ "'",
%%    Sql = Sql ++ "'" ++ T3 ++ "'",
%%    Sql = Sql ++ "'" ++ T4 ++ "'",
%%    Sql = Sql ++ "'" ++ T5 ++ "'",
%%    Sql = Sql ++ "'" ++ T6 ++ "'",
%%    Sql = Sql ++ "'" ++ T7 ++ "'",
%%    Sql = Sql ++ "'" ++ T8 ++ "'",
%%    Sql = Sql ++ "'" ++ T9 ++ "'",
%%    Sql = Sql ++ "'" ++ Ts ++ "'",
%%    Sql = Sql ++ "'" ++ Payload ++ "'",
%%    Sql = Sql ++ "'" ++ Topic ++ "'",
%%    Sql = Sql ++ "'" ++ From ++ "'",
%%    Sql = Sql ++ "'" ++ Qos ++ "'",
%%    Sql = Sql ++ "'" ++ Flags ++ "'",
%%    io:format("Sql ~s~n", [Sql]),
%%    Params = [T1,T2,T3,T4,T5,T6,T7,T8,T9, Ts, Payload, Topic, From, Qos, Flags],
%%    Parameters = string:join(Params, "','"),
    io:format("Parameters ~s~n", [Query]),
    CheckQuery = case emqx_message_persistence_pgsql_cli:equery(Query, []) of
                    {ok, [_Super], [{true}]} ->
                        io:format("1. super true "),
                        true;
                    {ok, [_Super], [_False]} ->
                        io:format("2. super false "),
                        false;
                    {ok, [_Super], []} ->
                        io:format("1. super [] "),
                        false;
                    {error, _Error} ->
                        io:format("1. error "),
                        false
                end,
    io:format("res ~s~n", [CheckQuery]).

check_if_exist(Ind, Lista) ->
  Length = length(Lista),
  if 
    Length >= Ind -> binary:bin_to_list(lists:nth(Ind, Lista));
    Length < Ind  -> ""
  end.


%% Called when the plugin application stop
unload() ->
    emqx:unhook('message.publish', fun ?MODULE:on_message_publish/2).

description() -> "Message Persistence PostgreSQL Module".

