# nmaglev

Erlang implementation of the consistent hashing maglev algorithm

## Introduction

MagLev is a [consistent hashing](https://en.wikipedia.org/wiki/Consistent_hashing) algorithm developed at Google and 
[published](https://static.googleusercontent.com/media/research.google.com/es//pubs/archive/44824.pdf) in 2016.

It aims to be lightning fast and always accurate due to the use of a deterministic algorithm at the spend 
of needing more computational power when there is a change in the list of possible outputs as it needs
to recalculate the internal hashing table.

## How to use it

1. Create your maglev table with the list of possible outputs: ```MaglevTable = nmaglev:create([a,b,c,d]).```
2. Use the table: ```Output = nmaglev:get(<<"some bin">>, MaglevTable).```


## About lookup table sizes

By default the lookup table size is 65537 as it should be enough for a normal scenario.
You can choose your own size using the nmaglev:create/2 function but please be aware that the size has to be
a prime number.

## Original paper

[https://static.googleusercontent.com/media/research.google.com/es//pubs/archive/44824.pdf](https://static.googleusercontent.com/media/research.google.com/es//pubs/archive/44824.pdf)