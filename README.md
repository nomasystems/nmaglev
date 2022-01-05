# nmaglev

![nmaglev](https://github.com/nomasystems/nmaglev/workflows/nmaglev/badge.svg?branch=master)

Erlang implementation of the load balancing maglev algorithm.

## Introduction

MagLev [\[1\]](#references) is a [load balancing](https://en.wikipedia.org/wiki/Load_balancing_(computing)) algorithm developed at Google and [published](https://static.googleusercontent.com/media/research.google.com/es//pubs/archive/44824.pdf) in 2016.

It aims to be lightning fast and always accurate due to the use of a deterministic algorithm at the spend 
of needing more computational power when there is a change in the list of possible outputs as it needs
to recalculate the internal hashing table.

## How to use it

1. Create your maglev table with the list of possible outputs: ```MaglevTable = nmaglev:create([a,b,c,d]).```
2. Use the table: ```Output = nmaglev:get(<<"some bin">>, MaglevTable).```

## About lookup table sizes

By default the lookup table size is 65537 as it should be enough for a normal scenario. You can choose your own size using the `nmaglev:create/2` function but please be aware that the size has to be a prime number.

## References

[1] [D. E. Eisenbud et al., “Maglev: A Fast and Reliable Software Network Load Balancer”, in Proceedings of the 13th Usenix Conference on Networked Systems Design and Implementation, Santa Clara, CA, 2016, bll 523–535.](https://research.google/pubs/pub44824/)
