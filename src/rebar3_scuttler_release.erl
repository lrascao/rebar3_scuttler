%%-*- mode: erlang -*-
-module(rebar3_scuttler_release).
-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, release).
-define(NAMESPACE, default).
-define(DEPS, [{?NAMESPACE, compile}]).

-define(PRE_START_HOOK_TEMPLATE, "pre_start_cuttlefish.tpl").

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
            {example, "rebar3 scuttler release"}, % How to use the plugin
            {opts, relx:opt_spec_list()},                   % list of options understood by the plugin
            {short_desc, "Rebar3 scuttler release plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State0) ->
    rebar_api:info("Running cuttlefish schema generator", []),
    ScuttlerConf = rebar_state:get(State0, scuttler, []),
    Relx = rebar_state:get(State0, relx, []),
    % find the release's name
    {release, {Name, _Vsn}, _} = lists:keyfind(release, 1, Relx),
    _ConfFilename =
        case lists:keyfind(conf_filename, 1, ScuttlerConf) of
           {conf_filename, ConfFilename0} ->
               ConfFilename0;
           _ ->
               io_lib:format("~s.conf", [Name])
        end,
    % find the cuttlefish escript executable
    CuttlefishBin =
        case filelib:is_regular(filename:join([rebar_dir:base_dir(State0), "bin", "cuttlefish"])) of
             true ->
                 filename:join([rebar_dir:base_dir(State0), "bin", "cuttlefish"]);
             false ->
                % it's either in _checkouts or some dep bin dir
                case filelib:wildcard(filename:join(["_checkouts", "cuttlefish*", "cuttlefish"])) ++
                    filelib:wildcard(filename:join(["_build", "*", "bin", "cuttlefish"])) of
                    [C | _] ->
                        C;
                    [] ->
                        % unable to find, give up
                        throw({no_cuttlefish_escript, rebar_dir:base_dir(State0)})
                end
         end,

    ReleaseDir = rebar_release_dir(State0),
    TargetDir = filename:join([ReleaseDir, Name]),
    Deps = rebar_state:all_deps(State0),
    Apps = rebar_state:project_apps(State0),

    SchemaDir =
        case lists:keyfind(schema_dir, 1, ScuttlerConf) of
           {schema_dir, SchemaDir0} ->
               SchemaDir0;
           false ->
               auto_discover
       end,
    AllSchemas = schemas(SchemaDir, Deps++Apps),

    % get the location of the plugin's extended start script hook,
    % should be in priv/cuttlefish_start_hook
    PreStartHook0 = filename:join([code:priv_dir(rebar3_scuttler), ?PRE_START_HOOK_TEMPLATE]),

    % the conf filename will use the name of this release and the .conf suffix
    ConfFile =
        case lists:keyfind(conf_file, 1, ScuttlerConf) of
           {conf_file, ConfFile0} ->
               ConfFile0;
           false ->
                atom_to_list(Name) ++ ".conf"
       end,

    % add template overlay entries that will copy all the cuttlefish schemas
    % and the binary to the release
    Overlays1 =
        case lists:keyfind(overlay, 1, Relx) of
            {overlay, Overlays0} ->
                overlays(Name, CuttlefishBin, AllSchemas) ++ Overlays0;
            _ ->
                overlays(Name, CuttlefishBin, AllSchemas)
        end,

    % find out the ebin dir for the plugin, we want to template our pre start
    % script and need somewhere to write it to
    PluginDeps = rebar_state:all_plugin_deps(State0),
    {ok, AppInfo} = rebar_app_utils:find(<<"rebar3_scuttler">>, PluginDeps),
    PluginEbinDir = rebar_app_info:ebin_dir(AppInfo),

    ConfigOutputFile =
        case lists:keyfind(output_file, 1, ScuttlerConf) of
           {output_file, ConfigOutputFile0} ->
                ConfigOutputFile0;
           false ->
                % no preference from the user, default
               "releases/{{release_version}}/cuttlefish.config"
       end,
    ConfigOutputFilename = filename:basename(ConfigOutputFile),
    ConfigOutputDir = filename:dirname(ConfigOutputFile),

    % lastly inject the template directive that will copy the plugin
    % pre start hook that will invoke cuttlefish bin and generate the
    % .config files of each schema
    % before that we need to replace all mustache variables in the
    % pre start script template
    % we'll template priv/pre_start_cuttlefish to the plugin's output dir
    PreStartHook = filename:join([PluginEbinDir, ?PRE_START_HOOK_TEMPLATE]),
    template(PreStartHook0, PreStartHook,
             [{"schema_dir", "share/schema"},
              {"output_dir", ConfigOutputDir},
              {"output_filename", ConfigOutputFilename},
              {"conf_file", ConfFile}]),
    TargetPreStartHook =
        case lists:keyfind(pre_start_hook, 1, ScuttlerConf) of
                   {pre_start_hook, TargetPreStartHook0} ->
                       TargetPreStartHook0;
                   false ->
                        % no preference from the user, default
                       "bin/hooks/pre_start_cuttlefish"
               end,
    Overlays = [{template, PreStartHook, TargetPreStartHook} | Overlays1],

    % override the `overlay` relx project section with additional entries
    % that will allow us to inject:
    %   - the cuttefish bin tool
    %   - the templated cuttefish schema files
    %   - the pre-start extended start script hook
    State = rebar_state:set(State0, relx,
                            lists:keydelete(overlay, 1, Relx) ++
                                [{overlay, Overlays}]),

    % generate the release, this will cause all schema files to be templated and copied
    % over to the release dir
    Res = rebar_relx:do(rlx_prv_release, "release", ?PROVIDER, State),

    % now that the schema files have been templated, we look them up
    % in order to generate a skeleton .conf file with all the defaults
    SchemaGlob = filename:join([TargetDir, "share", "schema", "*.schema"]),
    ReleaseSchemas = filelib:wildcard(SchemaGlob),

    case filelib:is_regular(filename:join([TargetDir, ConfFile])) of
        false ->
            % there is no .conf file present, we'll just generate one out of
            % the schema defaults
            case cuttlefish_schema:files(ReleaseSchemas) of
                {errorlist, _Es} ->
                    %% These errors were already printed
                    {error, "bad cuttlefish schemas"};
                {_Translations, Mappings, _Validators} ->
                    make_default_file(ConfFile, TargetDir, Mappings),
                    Res
            end;
        true ->
            Res
    end.

-spec format_error(any()) ->  iolist().
format_error({no_cuttlefish_escript, ProfileDir}) ->
    io_lib:format("No cuttlefish escript found under ~s or ~s", [filename:join(ProfileDir, "bin"),
                                                                "_build/default/bin"]);
format_error(Error) ->
    io_lib:format("~p", [Error]).

make_default_file(File, TargetDir, Mappings) ->
    Filename = filename:join([TargetDir, File]),
    filelib:ensure_dir(Filename),
    cuttlefish_conf:generate_file(Mappings, Filename),
    Filename.

schemas(auto_discover, Apps) ->
    lists:flatmap(fun(App) ->
                      AppDir = rebar_app_info:dir(App),
                      filelib:wildcard(filename:join(["{priv,schema,schemas}", "*.schema"]), AppDir) ++
                      filelib:wildcard(filename:join(["priv", "{schema,schemas}", "*.schema"]), AppDir)
                  end, Apps) ++ filelib:wildcard(filename:join(["{priv,schema,schemas}", "*.schema"]));
schemas(Dir, _) ->
    [filename:join([Dir, Schema]) || Schema <- filelib:wildcard("*.schema", Dir)].

overlays(_Name, CuttlefishBin, Schemas) ->
    SchemaOverlays = [begin
                            {template, Schema, filename:join(["share", "schema", filename:basename(Schema)])}
                      end || Schema <- Schemas],
    [{copy, CuttlefishBin, "bin/cuttlefish"},
     {mkdir, "share"},
     {mkdir, "share/schema"} | SchemaOverlays].

%% Determine if --output-dir or -o has been passed, and if so, use
%% that path for the release directory. Otherwise, default to the
%% default directory.
rebar_release_dir(State) ->
    DefaultRelDir = filename:join(rebar_dir:base_dir(State), "rel"),
    {Options, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(output_dir, Options) of
        undefined ->
            DefaultRelDir;
        OutputDir ->
            OutputDir
    end.

template(Source, Target, Context) ->
    {ok, Template} = file:read_file(Source),
    case catch bbmustache:render(Template, Context) of
        Bin when is_binary(Bin) ->
            file:write_file(Target, Bin);
        Error ->
            rebar_api:abort("failed generating ~p due to ~p",
                            [Target, Error]),
            {error, Error}
    end.

