%%-*- mode: erlang -*-
-module(rebar3_scuttler_utils).

-export([rebar_version/0]).

-spec rebar_version() -> verl:version_t().
rebar_version() ->
    {ok, Vsn0} = application:get_key(rebar, vsn),
    {ok, Vsn} = verl:parse(list_to_binary(Vsn0)),
    Vsn.

