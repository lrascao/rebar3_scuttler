%%-*- mode: erlang -*-
-module(rebar3_scuttler).

-export([init/1]).

init(State0) ->
    rebar3_scuttler_release:init(State0).
