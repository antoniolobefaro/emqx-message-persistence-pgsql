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

-module(emqx_message_persistence_pgsql_app).

-behaviour(application).

-emqx_plugin(auth).

-include("emqx_message_persistence_pgsql.hrl").

%% Application callbacks
-export([ start/2, stop/1]).

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    io:format("emqx_persistence_pgsql ~n"),
    {ok, Sup} = emqx_message_persistence_pgsql_sup:start_link(),
    emqx_message_persistence_pgsql:load(application:get_all_env()),
    {ok, Sup}.

stop(_State) ->
    emqx_message_persistence_pgsql:unload().
%    ok = emqx:unhook('client.authenticate', fun emqx_message_persistence_pgsql:check/3),
%    ok = emqx:unhook('client.check_acl', fun emqx_message_persistence_acl_pgsql:check_acl/5).

%%if_enabled(Par, Fun) ->
%%    case application:get_env(?APP, Par) of
%%        {ok, Query} -> Fun(parse_query(Par, Query));
%%        undefined   -> ok
%%    end.

