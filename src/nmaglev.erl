%%% Copyright (c) 2013 [Nomasystems, S.L. http://www.nomasystems.com]
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
%%%
-module(nmaglev).

%%% INCLUDE FILES

%%% EXTERNAL EXPORTS
-export([create/1, create/2, get/2]).

%%% MACROS
-define(DEFAULT_LOOKUP_SIZE, 65537).

%%% RECORDS

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
create(Nodes) ->
    LookupSize = ?DEFAULT_LOOKUP_SIZE,
    create(Nodes, LookupSize).

create(Nodes, LookupSize) ->
    NodesLen = length(Nodes),
    PermutationsTable = permutations(Nodes, LookupSize),
    lookup_map(PermutationsTable, NodesLen, LookupSize, {0, #{}, NodesLen}).

get(Key, Lookup) ->
    Offset = erlang:phash2(Key, maps:size(Lookup)),
    maps:get(Offset, Lookup).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
lookup_map(_PermutationsTable, _NodesLen, LookupSize, {Filled, FilledMap, _LastNodePos}) when
    Filled >= LookupSize
->
    FilledMap;
lookup_map(PermutationsTable, NodesLen, LookupSize, {Filled, FilledMap, LastNodePos}) ->
    {Permutation, ChosenNode, ChosenNodePos, NewPermutationsTable} = choose_node(
        PermutationsTable,
        NodesLen,
        FilledMap,
        LastNodePos
    ),
    lookup_map(
        NewPermutationsTable,
        NodesLen,
        LookupSize,
        {Filled + 1, FilledMap#{Permutation => ChosenNode}, ChosenNodePos}
    ).

choose_node(PermutationsTable, NodesLen, FilledMap, LastNodePos) ->
    NodePos = mod(LastNodePos, NodesLen) + 1,
    {Node, NodePermutations} = lists:nth(NodePos, PermutationsTable),
    [Permutation | RestNodePermutations] = NodePermutations,
    NewPermutationsTable = lists:keyreplace(
        Node,
        1,
        PermutationsTable,
        {Node, RestNodePermutations}
    ),
    case maps:is_key(Permutation, FilledMap) of
        false ->
            {Permutation, Node, NodePos, NewPermutationsTable};
        true ->
            choose_node(NewPermutationsTable, NodesLen, FilledMap, NodePos)
    end.

permutations(Nodes, LookupSize) ->
    [{Node, node_permutations(Node, LookupSize)} || Node <- Nodes].

node_permutations(Node, LookupSize) ->
    Offset = mod(erlang:phash(Node, LookupSize), LookupSize),
    Skip = mod(erlang:phash2(Node, LookupSize), LookupSize - 1) + 1,
    [mod(Offset + (Pos * Skip), LookupSize) || Pos <- lists:seq(0, LookupSize - 1)].

mod(A, B) when A > 0 ->
    A rem B;
mod(A, B) when A < 0 ->
    mod(A + B, B);
mod(0, _) ->
    0.
