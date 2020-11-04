%%-*- mode: erlang -*-
-module(rebar3_scuttler_tar).
-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, tar).
-define(NAMESPACE, default).
-define(DEPS, [{?NAMESPACE, release}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, ?NAMESPACE},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 scuttler tar"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Rebar3 scuttler release tarball plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State0) ->
    do_tar(State0).

-spec format_error(any()) ->  iolist().
format_error(Error) ->
    io_lib:format("~p", [Error]).

-spec do_tar(rebar_state:t()) -> rebar_state:t().
do_tar(State0) ->
    Vsn = rebar3_scuttler_utils:rebar_version(),
    rebar_api:debug("vsn: ~p", [Vsn]),
    case Vsn of
        #{minor := Minor} when Minor >= 14 ->
            % from 3.14 onwards, a relx refactor changed the interface
            % of rebar_relx
            rebar_relx:do(tar, State0);
        _ ->
            rebar_relx:do(rlx_prv_release, "tar", ?PROVIDER, State0)
    end.

