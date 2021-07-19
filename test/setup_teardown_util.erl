%%% Copyright (c) 2021 Nomasystems, S.L., All Rights Reserved
%%%
%%% This file contains Original Code and/or Modifications of Original Code as
%%% defined in and that are subject to the Nomasystems Public License version
%%% 1.0 (the 'License'). You may not use this file except in compliance with
%%% the License. BY USING THIS FILE YOU AGREE TO ALL TERMS AND CONDITIONS OF
%%% THE LICENSE. A copy of the License is provided with the Original Code and
%%% Modifications, and is also available at www.nomasystems.com/license.txt.
%%%
%%% The Original Code and all software distributed under the License are
%%% distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
%%% EXPRESS OR IMPLIED, AND NOMASYSTEMS AND ALL CONTRIBUTORS HEREBY DISCLAIM
%%% ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR
%%% NON-INFRINGEMENT. Please see the License for the specific language
%%% governing rights and limitations under the License.
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
