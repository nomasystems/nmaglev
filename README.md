# nmaglev
[![nmaglev ci](https://github.com/nomasystems/nmaglev/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/nmaglev/actions/workflows/ci.yml)

Erlang implementation of the consistent hashing algorithm used in the load balancing MagLev system.

## Introduction

MagLev [\[1\]](#references) is a network [load balancing](https://en.wikipedia.org/wiki/Load_balancing_(computing)) system developed at Google and [published](https://static.googleusercontent.com/media/research.google.com/es//pubs/archive/44824.pdf) in 2016. 

As part of the system a consistent hashing algorithm is proposed and that is what this library contains, an implementation of the consistent hashing algorithm part of the MagLev system.

It aims to be lightning fast and always accurate due to the use of a deterministic algorithm at the spend 
of needing more computational power when there is a change in the list of possible outputs as it needs
to recalculate the internal hashing table.

## How to use it

1. Create your maglev table with the list of possible outputs: ```MaglevTable = nmaglev:create([a,b,c,d]).```
2. Use the get function to retrieve the output for your input: ```Output = nmaglev:get(<<"some bin">>, MaglevTable).```

## About lookup table sizes

By default the lookup table size is 65537 as it should be enough for a normal scenario. You can choose your own size using the `nmaglev:create/2` function but please be aware that the size has to be a prime number.

## References

[1] [D. E. Eisenbud et al., “Maglev: A Fast and Reliable Software Network Load Balancer”, in Proceedings of the 13th Usenix Conference on Networked Systems Design and Implementation, Santa Clara, CA, 2016, bll 523–535.](https://research.google/pubs/pub44824/)
