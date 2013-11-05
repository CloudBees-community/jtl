-module(jtl).

-export([render/2]).

-define(MAX_TEMPLATE_MODULES, 1000).

render(Template, DataFile) when is_list(DataFile) ->
    render_template(Template, init_vars_from_file(DataFile));
render(Template, Json) when is_binary(Json) ->
    render_template(Template, init_vars_from_json(Json)).

init_vars_from_file(JsonFile) ->
    init_vars_read_file(file:read_file(JsonFile), JsonFile).

init_vars_read_file({ok, Bin}, _JsonFile) ->
    json_to_vars(jiffy:decode(Bin));
init_vars_read_file({error, Err}, JsonFile) ->
    error({read_file, JsonFile, Err}).

json_to_vars({L}) when is_list(L) ->
    [json_to_vars(X) || X <- L];
json_to_vars({Name, Attrs}) when is_binary(Name) ->
    {Name, json_to_vars(Attrs)};
json_to_vars(X) -> X.

init_vars_from_json(Bin) ->
    json_to_vars(jiffy:decode(Bin)).

render_template(Template, Vars) ->
    render_read_file(file:read_file(Template), Vars, Template).

render_read_file({ok, Bin}, Vars, Name) ->
    erlydtl_render(Bin, Vars, Name);
render_read_file({error, Err}, _Vars, Name) ->
    error({dtl_render, Name, Err}).

erlydtl_render(TemplateBin, Vars, TemplateName) ->
    M = template_module(TemplateBin),
    erlydtl:compile(TemplateBin, M),
    handle_render(M:render(Vars), TemplateName).

handle_render({ok, Rendered}, _Template) -> Rendered;
handle_render({error, Err}, Template) ->
    error({dtl_render, Template, Err}).

template_module(Bin) ->
    HashIndex = erlang:phash2(Bin, ?MAX_TEMPLATE_MODULES),
    list_to_atom("jtl_template_" ++ integer_to_list(HashIndex)).
