%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
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
-module(setup_teardown_util).

%%% SETUP/TEARDOWN SUITE EXPORTS
-export([setup_suite/1, teardown_suite/1]).

%%% MACROS
-define(MAX_TIME, 10000).

%%%-----------------------------------------------------------------------------
%%% SETUP/TEARDOWN SUITE EXPORTS
%%%-----------------------------------------------------------------------------
setup_suite(Conf) ->
    lists:foreach(fun(X) -> code:add_path(X) end, ct:get_config(paths, [])),
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>> = crypto:strong_rand_bytes(
        12
    ),
    rand:seed(exsplus, {I1, I2, I3}),
    dbg:tracer(),
    dbg:p(all, [c, sos, sol, timestamp]),
    Apps = ct:get_config(apps, []),
    Env = ct:get_config(env, []),
    [ok = application:load(App) || App <- Apps],
    [ok = application:set_env(App, K, V) || {App, KeyVal} <- Env, {K, V} <- KeyVal],
    [ok = application:start(App) || App <- Apps],
    MaxTime = ct:get_config(max_time, ?MAX_TIME),
    [{max_time, MaxTime} | Conf].

teardown_suite(_Conf) ->
    Apps = ct:get_config(apps, []),
    [ok = application:stop(App) || App <- lists:reverse(Apps)],
    [ok = application:unload(App) || App <- lists:reverse(Apps)],
    ok.
