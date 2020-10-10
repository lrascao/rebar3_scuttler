rebar3 scuttlefish plugin
=====

![Build Status](https://github.com/lrascao/rebar3_scuttler/workflows/CI/badge.svg)
[![hex.pm version]()](https://hex.pm/packages/rebar3_appup_plugin)

A rebar3 plugin for [cuttlefish](https://github.com/Kyorai/cuttlefish) schema handling with a slightly different approach than [rebar3_cuttlefish](https://github.com/vernemq/rebar3_cuttlefish).
It departs from the assumption that your Erlang release is already making use of `sys.config` and other `.config` files and you simply want to expose a few specific parameters
in a non-Erlanger `.conf` file for your application users to tweak. 

A cuttlefish recap
-----

[Basho's original cuttlefish project](https://github.com/basho/cuttlefish) came up with the idea of allowing non-Erlanger users to tweak any Erlang application's parameters without
having to understand the complicated proplist syntax. A user simply needs to edit a human readable `.conf` file (like `sysctl.conf`, hence the `cuttlefish` name) and run the application.

Requirements
-----

For the plugin to work you'll need to enable [extended start scripts](http://rebar3.org/docs/deployment/releases/#extended-start-script), `rebar3_scuttler` relies on
[hooks](http://rebar3.org/docs/deployment/releases/#hooks) in order to generate the cuttlefish schema `.config` output files.
    
Integration
-----
    
In order to integrate and make use of this plugin the following steps are needed:

* Include the `rebar3_scuttler` plugin in your project
* Write a [cuttlefish schema](https://github.com/basho/cuttlefish/wiki/Cuttlefish-for-Erlang-Developers) that exposes the application parameters
   you'll allow your users to change.
* Configure the `rebar3_scuttler` plugin
* Add the cuttlefish pre start hook to your release's hooks
* Include the generated `.config` files from your `sys.config`

### Including the plugin

Add the plugin to your project's rebar.config:

```
% this informs rebar3 that the rebar3_scuttler plugin will take over
% the release generation process, this is necessary so that it is able
% to inject the cuttlefish invocation to the pre-start release hook
{project_plugins, [
    {rebar3_scuttler,
        {git, "",
            {branch, "master"}}}
]}.
```

### Writing the cuttlefish schema

Basho's original [cuttlefish project documentation](https://github.com/basho/cuttlefish/wiki/Cuttlefish-for-Erlang-Developers) has everything needed.

Here is an example of configuration for the `lhttpc` application:

```
%% @doc Number of default workers for each lhttpc pool
{mapping, "lhttpc.pool_size", "lhttpc.pool_size", [
    {default, 10},
    {datatype, integer}
]}.
```

Files containing cuttlefish schemas should have the `.schema` extension, you should source control them along with your application
(a typical place to store them is beneath the `priv` dir).

### Configuring the plugin

`rebar3_scuttler` has the following options:

* `schema_dir`, can either be the atom `auto_discover` or a string containing a dir
   where the plugin will recursively look for `.schema` files.
   Use `auto_discover` when your dependencies also offer cuttlefish schemas
   that you want to make use of.
* `pre_start_hook`, this is the name of file that will contain this plugin's pre start hook to be invoked just
  prior to starting a release.
  This script will take care of processing `.schema` and `.conf` files in order to output
  `.config` files that you will be able to include from your own.
* `output_file`, the `.config` file that gets generated as the final result
* `conf_file`, the human non-Erlanger `.conf` that your application users will be able to tweak.

The following entries should go in `rebar.config`.

```
% scuttler plugin opts
{scuttler, [
    % find cuttlefish schemas in your project and it's
    % dependencies:
    %   * auto_discover, finds *.schema files in:
    %                    priv/*.schema
    %                    priv/schema/*.schema
    %                    priv/schemas/*.schema
    %                    schema/*.schema
    %                    schemas/*.schema
    %   * "<dir>", find all *.schema in dir
    %
    % all of the discovered schemas will end up in the release under
    % `share/schema`
    {schema_dir, "priv/schemas"},

    % Specifies where you'd like rebar3_scuttler to generate
    % the pre start hook to. This is intended to be then added
    % to the extended_start_script_hooks/pre_start relx entry list
    % for it to be invoked prior to the release start
    {pre_start_hook, "bin/hooks/pre_start_cuttlefish"},

    % Specifies the .config filename that cuttlefish generates
    % out of each schema. This filename is then intended to be
    % included in your sys.config file.
    % eg. sys.config
    % [
    %     {myapp, [
    %           {my_conf1, value}
    %     ]},
    %
    %     "releases/{{release_version}}/config/generated/user_defined.config"
    % ].
    {output_file, "releases/{{release_version}}/config/generated/user_defined.config"},

    % this is the human readable .conf file that the users of your application
    % will understand and edit in order to change configuration parameters,
    % it's location is relative to the root dir of the release
    % (ie. alongside bin, releases, etc)
    {conf_file, "etc/simple_web_server.conf"}
]}.
```

### Adding the pre start hook

Create or add the `extended_start_script_hooks` entry to `rebar.config` `relx`'s section, if you were already
using it's just a matter of adding another `custom` entry to `pre_start`.

```
    % start script hooks
    {extended_start_script_hooks, [
        {pre_start, [
          % besides our own pre start script, we're here adding
          % the one that was generated out of rebar3_scuttler,
          % this script will pick up any .schema file in share/schema
          % and generate a same name .config file in `output_dir`
          %
          % notice that the name here matches the one we defined above in
          % scuttler.pre_start_hook, it's just missing the `bin` prefix because
          % start script hooks are relative to the extended start script location.
          {custom, "hooks/pre_start_cuttlefish"}
        ]}
    ]}
```

### Including generated files

Finally, in your `sys.config` file you can include the generated `.config` file as [per the OTP doc](https://erlang.org/doc/man/config.html).

```
 [
     {myapp, [
           {my_conf1, value}
     ]},

     "releases/{{release_version}}/config/generated/user_defined.config"
 ].
```

Copyright and License
-----

Copyright (c) 2020 Luis Rascão

**rebar3_scuttler** source code is licensed under [MIT](LICENSE).
