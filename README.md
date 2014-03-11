Nmap Service Discovery Spoofer
==============================

Hosts fake TCP services of choice.

Building
========

- Get Erlang and rebar for your machine
- Run make to fetch latest service-probes data
- `rebar compile`

Running
=======

From a shell (`erl -pa ebin`) do `nnop_port:run(FromPort,ToPort,WhiteList,BlackList).`
