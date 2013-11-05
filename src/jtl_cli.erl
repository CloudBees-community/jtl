-module(jtl_cli).

-export([exec/1]).

-define(prog, "jtl").
-define(spec,
        [{output, $o, "output", string, "Write output to file"},
         {help, $h, "help", boolean, "Print this message and exit"}]).
-define(args_help, "TEMPLATE VARS").

-record(exec, {template, data_json, output}).

exec(Args) ->
    exec_parsed(parse_args(Args)).

parse_args(Args) ->
    getopt:parse(?spec, Args).

exec_parsed({ok, {Opts, Args}}) ->
    exec_or_usage(help_option(Opts), Opts, Args);
exec_parsed({error, Err}) ->
    cli_error(Err).

help_option(Opts) ->
    case proplists:get_bool(help, Opts) of
        true -> usage;
        false -> exec
    end.

exec_or_usage(usage, _Opts, _Args) ->
    usage_ok();
exec_or_usage(exec, Opts, Args) ->
    handle_validated_args(validate_args(Opts, Args)).

validate_args(Opts, [Template, DataJson]) ->
    #exec{
       template=Template,
       data_json=DataJson,
       output=output_option(Opts)};
validate_args(_, _) ->
    error.

output_option(Opts) ->
    proplists:get_value(output, Opts, stdout).

handle_validated_args(#exec{}=Args) ->
    #exec{
       template=Template,
       data_json=DataJson,
       output=Output} = Args,
    try jtl:render(Template, DataJson) of
        Rendered -> handle_rendered(Rendered, Output)
    catch
        _:Err -> handle_error(Err, Args)
    end;
handle_validated_args(error) ->
    usage_error().

handle_rendered(Rendered, stdout) ->
    io:format("~s~n", [Rendered]);
handle_rendered(Rendered, File) ->
    handle_write_file(file:write_file(File, Rendered), File).

handle_write_file(ok, _File) -> ok;
handle_write_file({error, Err}, File) ->
    {error, write_file_error_msg(Err, File)}.

write_file_error_msg(Err, File) ->
    io_lib:format("Error writing ~s: ~p", [File, Err]).

handle_error({error, Err}, Args) ->
    handle_error(Err, Args);
handle_error({Pos, invalid_json}, #exec{data_json=File}) ->
    {error, invalid_json_error_msg(Pos, File)};
handle_error({Pos, erlydtl_parser, Msg}, #exec{template=File}) ->
    {error, template_compile_error_msg(Pos, Msg, File)};
handle_error(Err, _Args) ->
    {error, general_error_msg(Err)}.

invalid_json_error_msg(Pos, File) ->
    io_lib:format("Invalid JSON in ~s at char pos ~p", [File, Pos]).

template_compile_error_msg(Pos, Msg, File) ->
    io_lib:format("Template error in ~s at char pos ~p: ~s", [File, Pos, Msg]).

general_error_msg(Err) ->
    Trace = erlang:get_stacktrace(),
    io_lib:format("Unhandled error: ~p~n~p", [Err, Trace]).

cli_error(Err) -> {error, Err}.

usage_ok() ->
    getopt:usage(?spec, ?prog, ?args_help),
    ok.

usage_error() ->
    getopt:usage(?spec, ?prog, ?args_help),
    error.
