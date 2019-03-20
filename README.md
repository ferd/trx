trx
=====

A rebar plugin to export Erlang test data into Visual Studio test format (`.trx` files)

Use
---

Add the plugin to your rebar config:

    {project_plugins, [
        {trx, ".*", {git, "https://github.com/ferd/trx.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application to generate a report based on the latest common test run:

    $ rebar3 as test trx
    ===> Fetching trx
    ===> Compiling trx
    <Plugin Output>

Or do it automatically with a Common Test hook:

    {ct_opts, [{ct_hooks, [cth_trx]}]}.

These functions will generate a report under the right profile in the profile directory, under the latest Common Test name (like `ct_run.nonode@nohost.2017-11-03_14.40.29.trx`). To force a different name, declare the hook as:

    {ct_opts, [{ct_hooks, [{cth_trx, [{filename, "latest.trx"}]}]}]}.

And the file will instead bear that name (the path remains relative).

TODO
----

- Eunit integration
- PropEr integration (is it even possible?)
- Code coverage?
