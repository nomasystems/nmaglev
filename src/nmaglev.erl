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
%%%
-module(nmaglev).

%%%  SPECS
-include("nmaglev_specs.hrl").

%%% EXTERNAL EXPORTS
-export([create/1, create/2, get/2]).

%%% MACROS
-define(DEFAULT_LOOKUP_SIZE, 65537).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
create(Outputs) ->
    LookupSize = ?DEFAULT_LOOKUP_SIZE,
    create(Outputs, LookupSize).

create(Outputs, LookupSize) ->
    OutputsLen = erlang:length(Outputs),
    PermutationsTable = permutations(Outputs, LookupSize),
    lookup_map(PermutationsTable, OutputsLen, LookupSize, {0, #{}, OutputsLen}).

get(Key, Lookup) ->
    Offset = erlang:phash2(Key, maps:size(Lookup)),
    maps:get(Offset, Lookup).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
lookup_map(_PermutationsTable, _OutputsLen, LookupSize, {Filled, FilledMap, _LastOutputPos}) when
    Filled >= LookupSize
->
    FilledMap;
lookup_map(PermutationsTable, OutputsLen, LookupSize, {Filled, FilledMap, LastOutputPos}) ->
    {Permutation, ChosenOutput, ChosenOutputPos, NewPermutationsTable} = choose_output(
        PermutationsTable,
        OutputsLen,
        FilledMap,
        LastOutputPos
    ),
    lookup_map(
        NewPermutationsTable,
        OutputsLen,
        LookupSize,
        {Filled + 1, FilledMap#{Permutation => ChosenOutput}, ChosenOutputPos}
    ).

choose_output(PermutationsTable, OutputsLen, FilledMap, LastOutputPos) ->
    OutputPos = mod(LastOutputPos, OutputsLen) + 1,
    {Output, OutputPermutations} = lists:nth(OutputPos, PermutationsTable),
    [Permutation | RestOutputPermutations] = OutputPermutations,
    NewPermutationsTable = lists:keyreplace(
        Output,
        1,
        PermutationsTable,
        {Output, RestOutputPermutations}
    ),
    case maps:is_key(Permutation, FilledMap) of
        false ->
            {Permutation, Output, OutputPos, NewPermutationsTable};
        true ->
            choose_output(NewPermutationsTable, OutputsLen, FilledMap, OutputPos)
    end.

permutations(Outputs, LookupSize) ->
    [{Output, output_permutations(Output, LookupSize)} || Output <- Outputs].

output_permutations(Output, LookupSize) ->
    Offset = mod(fold32(crypto:hash(sha, Output)), LookupSize),
    Skip = mod(erlang:phash2(Output, LookupSize), LookupSize - 1) + 1,
    [mod(Offset + (Pos * Skip), LookupSize) || Pos <- lists:seq(0, LookupSize - 1)].

mod(A, B) when A > 0 ->
    A rem B;
mod(A, B) when A < 0 ->
    mod(A + B, B);
mod(0, _) ->
    0.

fold32(Data) ->
    fold32(Data, 0).

fold32(<<H:32, T/binary>>, Hash) ->
    fold32(T, Hash bxor H);
fold32(<<H:24>>, Hash) ->
    Hash bxor H;
fold32(<<H:16>>, Hash) ->
    Hash bxor H;
fold32(<<H:8>>, Hash) ->
    Hash bxor H;
fold32(<<>>, Hash) ->
    Hash.
