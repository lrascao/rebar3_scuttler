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
-define(DEFAULT_PRE_START_CUTTLEFISH_HOOK_SCRIPT, "bin/hooks/pre_start_cuttlefish").
-define(ERLANG_VM_ARGS_SCHEMA, "erlang.vm.args.schema").

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
            {opts, []},                   % list of options understood by the plugin
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

    % find deps and apps for this project
    ReleaseDir = rebar_release_dir(State0),
    TargetDir = filename:join([ReleaseDir, Name]),
    Deps = rebar_state:all_deps(State0),
    Apps = rebar_state:project_apps(State0),

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

    % find out the ebin dir for the plugin, we want to template our pre start
    % script and need somewhere to write it to
    PluginDeps = rebar_state:all_plugin_deps(State0),
    {ok, AppInfo} = rebar_app_utils:find(<<"rebar3_scuttler">>, PluginDeps),
    PluginEbinDir = rebar_app_info:ebin_dir(AppInfo),

    % get the location of the plugin's extended start script hook,
    % should be in priv/pre_start_cuttlefish.tpl
    PreStartHookTemplate = filename:join([code:priv_dir(rebar3_scuttler), ?PRE_START_HOOK_TEMPLATE]),

    % the conf filename will default the name of this release and the .conf suffix unless
    % requested by the developer
    ConfFile = proplists:get_value(conf_file, ScuttlerConf,
                                   atom_to_list(Name) ++ ".conf"),

    % get the list of configured schema transformation definitions
    SchemaDefs = proplists:get_value(schemas, ScuttlerConf, []),
    % get the name of the to be generated pre start hook
    ReleasePreStartHook = proplists:get_value(pre_start_hook, ScuttlerConf, 
                                              ?DEFAULT_PRE_START_CUTTLEFISH_HOOK_SCRIPT),

    % get the projects overlays and apply the overlay that will copy over the cuttlefish bin
    % and create the share/schema directory inside the release
    Overlays0 = apply_bin_overlay(Name, CuttlefishBin, proplists:get_value(overlay, Relx, [])),

    % go through each of the requested schema transformation definition
    % and generate a file for each of them
    {AllSchemas, Overlays1, PreStartHookBin} =
        lists:foldl(fun(SchemaDef, {SchemasAcc0, OverlaysAcc0, PreStartHookBinAcc0}) ->
                        #{schemas := Schemas,
                          release_schema_dir := ReleaseSchemaDir,
                          output_file := OutputFile} = schemas(SchemaDef, Deps++Apps, State0),

                        % add template overlay entries that will copy all the cuttlefish schemas
                        % to the release
                        OverlaysAcc = apply_schema_overlays(Schemas, ReleaseSchemaDir, OverlaysAcc0),

                        OutputFilename = filename:basename(OutputFile),
                        OutputDir = filename:dirname(OutputFile),

                        % lastly inject the template directive that will copy the plugin
                        % pre start hook that will invoke cuttlefish bin and generate the
                        % .config files of each schema
                        % before that we need to replace all mustache variables in the
                        % pre start script template
                        PreStartHookBin = template({file, PreStartHookTemplate}, 
                                                   [{"schema_dir", ReleaseSchemaDir},
                                                    {"output_dir", OutputDir},
                                                    {"output_filename", OutputFilename},
                                                    {"conf_file", ConfFile}]),

                        {Schemas ++ SchemasAcc0, OverlaysAcc, <<PreStartHookBinAcc0/binary, PreStartHookBin/binary>>} 
                    end,
                    {[], Overlays0, <<"">>},
                    SchemaDefs),
    rebar_api:debug("overlays: ~p",
                     [Overlays1]),

    % write the templated result file to the plugin's output dir
    PreStartHookFile = filename:join([PluginEbinDir, ?PRE_START_HOOK_TEMPLATE]),
    ok = file:write_file(PreStartHookFile, PreStartHookBin),
    Overlays = [{template, PreStartHookFile, ReleasePreStartHook} | Overlays1],

    % override the `overlay` relx project section with additional entries
    % that will allow us to inject:
    %   - the cuttefish bin tool
    %   - the templated cuttefish schema files
    %   - the pre-start extended start script hook
    State1 = rebar_state:set(State0, relx,
                             lists:keydelete(overlay, 1, Relx) ++
                                             [{overlay, Overlays}]),

    % override the `extended_start_script_hooks` relx section, this allows us
    % to add the release pre start hook to the list of hooks
    StartScriptHooks0 = proplists:get_value(extended_start_script_hooks, Relx, []),
    PreStartHooks = [{custom, ReleasePreStartHook} |
                        proplists:get_value(pre_start, StartScriptHooks0, [])],
    StartScriptHooks =
        lists:keydelete(pre_start, 1, StartScriptHooks0) ++ [{pre_start, PreStartHooks}],
    State2 = rebar_state:set(State1, relx,
                             lists:keydelete(extended_start_script_hooks, 1, Relx) ++
                                             [{extended_start_script_hooks, StartScriptHooks}]),

    % generate the release, this will cause all schema files to be templated and copied
    % over to the release dir
    State3 = do_release(State2),

    rebar_api:debug("all schemas: ~p", [AllSchemas]),
    % % now that the schema files have been templated, we look them up
    % % in order to generate a skeleton .conf file with all the defaults
    ReleaseSchemas = find_release_schemas(TargetDir, AllSchemas),
    rebar_api:debug("release schemas: ~p", [ReleaseSchemas]),

    case filelib:is_regular(filename:join([TargetDir, ConfFile])) of
        false ->
            % there is no .conf file present, we'll just generate one out of
            % the schema defaults
            case cuttlefish_schema:files(ReleaseSchemas) of
                {errorlist, Errors} ->
                    rebar_api:error("bad cuttlefish schemas: ~p (~p)",
                                    [ReleaseSchemas, Errors]),
                    %% These errors were already printed
                    {error, "bad cuttlefish schemas"};
                {_Translations, Mappings, _Validators} ->
                    make_default_file(ConfFile, TargetDir, Mappings),
                    % now a bit of trickery that should be justified. Earlier we asked relx
                    % to create the release and gave it the schema overlays to copy over to the
                    % final release. Only after that did we ask cuttlefish to generate a default conf file
                    % based on these schema files, now we need to sneak a copy overlay of this generated
                    % conf file into relx's state in order for it to end up in a tarball thats a result of
                    % `rebar3 tar`.
                    {ok, rebar_state:set(State3, relx,
                                         lists:keydelete(overlay, 1, Relx) ++
                                                         [{overlay, [
                                                            {copy, ConfFile, ConfFile} | Overlays]}])}
            end;
        true ->
            rebar_api:debug("no need to generate a default .conf file, one already exists at ~p",
                            [filename:join([TargetDir, ConfFile])]),
            {ok, rebar_state:set(State3, relx,
                                 lists:keydelete(overlay, 1, Relx) ++
                                                 [{overlay, [
                                                    {copy, ConfFile, ConfFile} | Overlays]}])}
    end.

-spec do_release(rebar_state:t()) -> rebar_state:t().
do_release(State0) ->
    Vsn = rebar3_scuttler_utils:rebar_version(),
    {ok, State} =
        case Vsn of
            #{minor := Minor} when Minor >= 14 ->
                % from 3.14 onwards, a relx refactor changed the interface
                % of rebar_relx
                rebar_relx:do(release, State0);
            _ ->
                rebar_relx:do(rlx_prv_release, "release", ?PROVIDER, State0)
        end,
    State.

find_release_schemas(Dir, Schemas) ->
    SchemaFilenames = lists:map(fun filename:basename/1, Schemas),
    %% Convert simple extension to proper regex
    SchemaExtRe = "^[^._].*\\" ++ ".schema" ++ [$$],
    % recursively look for all .schema files in the supplied dir
    SchemasFound = rebar_utils:find_files(Dir, SchemaExtRe, true),
    % now filter out all the files that were not provided in the configuration
    lists:filter(fun(SchemaFound) ->
                    % ignore all .schema located beneath the `lib` dir since these
                    % contain .schema files that were templated by relx and might contain
                    % unprocessed overlay vars
                    case lists:prefix(filename:join([Dir, "lib"]), SchemaFound) of
                        true ->
                            false;
                        _ ->
                            lists:member(filename:basename(SchemaFound), SchemaFilenames)
                    end
                 end,
                 SchemasFound).

-spec format_error(any()) ->  iolist().
format_error({no_cuttlefish_escript, ProfileDir}) ->
    io_lib:format("No cuttlefish escript found under ~s or ~s", [filename:join(ProfileDir, "bin"),
                                                                "_build/default/bin"]);
format_error(Error) ->
    io_lib:format("~p", [Error]).

make_default_file(File, TargetDir, Mappings) ->
    rebar_api:debug("generating default file ~p in ~p",
                    [File, TargetDir]),
    Filename = filename:join([TargetDir, File]),
    filelib:ensure_dir(Filename),
    cuttlefish_conf:generate_file(Mappings, Filename),
    Filename.

schemas({vm_args, OutputFile}, _Apps, _State) ->
    #{schemas => [filename:join([code:priv_dir(rebar3_scuttler), ?ERLANG_VM_ARGS_SCHEMA])],
      release_schema_dir => "releases/{{release_version}}",
      output_file => OutputFile};
schemas({auto_discover, ReleaseSchemaDir, OutputFile}, Apps, _State) ->
    Schemas = lists:flatmap(fun(App) ->
                              AppDir = rebar_app_info:dir(App),
                              filelib:wildcard(filename:join(["{priv,schema,schemas}", "*.schema"]), AppDir) ++
                              filelib:wildcard(filename:join(["priv", "{schema,schemas}", "*.schema"]), AppDir)
                            end,
                            Apps) ++
              filelib:wildcard(filename:join(["{priv,schema,schemas}", "*.schema"])),
    #{schemas => Schemas,
      release_schema_dir => ReleaseSchemaDir,
      output_file => OutputFile};
schemas({Dir0, ReleaseSchemaDir, OutputFile}, _, State) when is_list(Dir0) ->
    Dir = template(Dir0, 
                   [{"deps_dir", rebar_dir:deps_dir(State)}]),
    rebar_api:debug("looking for *.schemas in ~p",
                    [Dir]),
    #{schemas => [filename:join([Dir, Schema]) || Schema <- filelib:wildcard("*.schema", Dir)],
      release_schema_dir => ReleaseSchemaDir,
      output_file => OutputFile}.

apply_bin_overlay(_Name, CuttlefishBin, Overlays0) ->
    [{copy, CuttlefishBin, "bin/cuttlefish"}] ++ Overlays0.

apply_schema_overlays(Schemas, SchemasDir, Overlays0) ->
    SchemaOverlays = [begin
                            {template, Schema, filename:join([SchemasDir, filename:basename(Schema)])}
                      end || Schema <- Schemas],
    apply_mkdir_overlay(SchemasDir, SchemaOverlays ++ Overlays0).

apply_mkdir_overlay(undefined, Overlays0) ->
    Overlays0;
apply_mkdir_overlay(Dir, Overlays0) ->
    [{mkdir, Dir} | Overlays0].

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

template({file, Source}, Context) ->
    {ok, Template} = file:read_file(Source),
    case catch bbmustache:render(Template, Context) of
        Bin when is_binary(Bin) ->
            Bin;
        Error ->
            rebar_api:abort("failed generating due to ~p",
                            [Error]),
            {error, Error}
    end;
template(Template, Context) when is_list(Template) ->
    binary_to_list(template(list_to_binary(Template), Context));
template(Template, Context) when is_binary(Template) ->
    bbmustache:render(Template, Context).

