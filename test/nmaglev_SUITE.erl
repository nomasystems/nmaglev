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
-module(nmaglev_SUITE).

-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([nowarn_export_all, export_all]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [usage, consistency, distribution].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    nct_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
usage() ->
    [{userdata, [{doc, "Tests the usage of the library"}]}].

usage(_Conf) ->
    Nodes = ["node" ++ erlang:integer_to_list(N) || N <- lists:seq(0, 50)],
    MaglevTable = nmaglev:create(Nodes),

    NodeForKey1 = nmaglev:get(<<"key1">>, MaglevTable),
    NodeForKey2 = nmaglev:get(<<"key2">>, MaglevTable),
    NodeForKey3 = nmaglev:get(<<"key3">>, MaglevTable),

    ?assertEqual(NodeForKey1, nmaglev:get(<<"key1">>, MaglevTable)),
    ?assertEqual(NodeForKey2, nmaglev:get(<<"key2">>, MaglevTable)),
    ?assertEqual(NodeForKey3, nmaglev:get(<<"key3">>, MaglevTable)),

    NewNodes = lists:delete(NodeForKey1, Nodes),
    NewMaglevTable = nmaglev:create(NewNodes),

    ?assertNotEqual(NodeForKey1, nmaglev:get(<<"key1">>, NewMaglevTable)),
    ?assertEqual(NodeForKey2, nmaglev:get(<<"key2">>, NewMaglevTable)),
    ?assertEqual(NodeForKey3, nmaglev:get(<<"key3">>, NewMaglevTable)),

    ok.

distribution() ->
    [{userdata, [{doc, "Tests the distribution of the maglev hashing algorithm"}]}].

distribution(_Conf) ->
    StartTime = erlang:timestamp(),
    Nodes = ["node" ++ erlang:integer_to_list(N) || N <- lists:seq(0, 50)],
    MaglevMap = nmaglev:create(Nodes),
    Distribution = lists:foldl(
        fun({_Permutation, Node}, Acc) ->
            case Acc of
                #{Node := Count} ->
                    Acc#{Node => Count + 1};
                Acc ->
                    Acc#{Node => 1}
            end
        end,
        #{},
        maps:to_list(MaglevMap)
    ),
    ct:print("Maglev distribution: ~p", [Distribution]),
    DistributionList = maps:to_list(Distribution),
    CheckDeviation = fun(NodeCount) ->
        Fun = fun({_OtherNode, OtherNodeCount}) ->
            Deviation = (erlang:abs((NodeCount - OtherNodeCount) / NodeCount) * 100),
            Deviation < 10
        end,
        lists:all(Fun, DistributionList)
    end,
    [true = CheckDeviation(NodeCount) || {_Node, NodeCount} <- DistributionList],
    Time = timer:now_diff(erlang:timestamp(), StartTime),
    ct:print("Distribution test elapsed time: ~p ms", [(Time / 1000)]),
    ok.

consistency() ->
    [{userdata, [{doc, "Tests the consistency of the maglev hashing algorithm"}]}].

consistency(_Conf) ->
    StartTime = erlang:timestamp(),
    [ok = consistency_test(NodesNum) || NodesNum <- lists:seq(2, 200)],
    Time = timer:now_diff(erlang:timestamp(), StartTime),
    ct:print("Consistency test elapsed time: ~p ms", [(Time / 1000)]),
    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
consistency_test(NodesNum) ->
    Nodes = ["node" ++ erlang:integer_to_list(N) || N <- lists:seq(0, NodesNum)],
    MaglevMap = nmaglev:create(Nodes, 997),
    Node = nmaglev:get(self(), MaglevMap),

    NodesWithoutOtherNode = remove_other_node(Nodes, Node),
    MaglevMapWithoutOneNode = nmaglev:create(NodesWithoutOtherNode, 997),
    true = Node == nmaglev:get(self(), MaglevMapWithoutOneNode),
    NewNode = "node" ++ erlang:integer_to_list(NodesNum + 1),
    MaglevMapPlusOneNode = nmaglev:create(Nodes ++ [NewNode], 997),
    ok =
        case nmaglev:get(self(), MaglevMapPlusOneNode) of
            Node ->
                ok;
            NewNode ->
                ok;
            Other ->
                {ko, Other}
        end,
    ok.

remove_other_node([Node1, Node2 | Rest], NodeToKeep) ->
    case NodeToKeep of
        Node1 ->
            [Node1 | Rest];
        _ ->
            [Node2 | Rest]
    end.
