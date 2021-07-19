%%% Copyright (c) 2009 Nomasystems, S.L., All Rights Reserved
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% o Redistributions of source code must retain the above copyright notice,
%%%   this list of conditions and the following disclaimer.
%%%
%%% o Redistributions in binary form must reproduce the above copyright notice,
%%%   this list of conditions and the following disclaimer in the documentation
%%%   and/or other materials provided with the distribution.
%%%
%%% o Neither the name of ERLANG TRAINING AND CONSULTING nor the names of its
%%%   contributors may be used to endorse or promote products derived from this
%%%   software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
-module(nmaglev_SUITE).

%%% INCLUDE FILES

%%% EXTERNAL EXPORTS
-compile([nowarn_export_all, export_all]).
-define(TIMES, 1000000).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [consistency, distribution].

sequences() ->
    [].

suite() ->
    [{timetrap, {minutes, 60}}].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    setup_teardown_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    setup_teardown_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    tracing_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    tracing_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
distribution() ->
    [{userdata, [{doc, "Tests the distribution of the maglev hashing algorithm"}]}].

distribution(_Conf) ->
    Nodes = ["node" ++ integer_to_list(N) || N <- lists:seq(0, 50)],
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
            Deviation = (abs((NodeCount - OtherNodeCount) / NodeCount) * 100),
            Deviation < 10
        end,
        lists:all(Fun, DistributionList)
    end,
    [true = CheckDeviation(NodeCount) || {_Node, NodeCount} <- DistributionList],
    ok.

consistency() ->
    [{userdata, [{doc, "Tests the consistency of the maglev hashing algorithm"}]}].

consistency(_Conf) ->
    [ok = consistency_test(NodesNum) || NodesNum <- lists:seq(2, 200)].

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
consistency_test(NodesNum) ->
    Nodes = ["node" ++ integer_to_list(N) || N <- lists:seq(0, NodesNum)],
    MaglevMap = nmaglev:create(Nodes, 997),
    Node = nmaglev:get(self(), MaglevMap),

    NodesWithoutOtherNode = remove_other_node(Nodes, Node),
    MaglevMapWithoutOneNode = nmaglev:create(NodesWithoutOtherNode, 997),
    true = Node == nmaglev:get(self(), MaglevMapWithoutOneNode),
    NewNode = "node" ++ integer_to_list(NodesNum + 1),
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
