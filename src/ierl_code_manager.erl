%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc The code manager handles execution of erlang code and
%%%      compilation of erlang modules.
%%%
%%% @end
%%% Created : 31. Mar 2014 10:02
%%%-------------------------------------------------------------------
-module (ierl_code_manager).
-export ([module_or_expression/2]).

%% @spec module_or_expression(list(), list()) -> tuple()
%% @doc Determines whether the incoming code is a module that is to be compiled
%%      or an erlang expression to be executed.
module_or_expression([$-,$m,$o,$d,$u,$l,$e,$(|RestOfCode], _Bindings) ->
  % If code is a module, do the following steps:
  % 1. Get module name.
  % 2. Save code to file
  % 3. Compile in [Some folder name]
  % 4. Capture and return compilation output

  % 1. Get module name
  ModuleName = get_module_name_from_code(RestOfCode),
  print:line(ModuleName),

  % 2. Save file in some folder TODO - Make separate folder for modules
  FullCode = lists:append("-module(", RestOfCode),
  FileName = lists:append(ModuleName, ".erl"),
  {ok, IODevice} = file:open(FileName, [write]), file:write(IODevice, FullCode), file:close(IODevice),

  print:line("Trying to compile module"),
  % 3 + 4. Compile Module, Capture Result and Return
  CompileResult = os:cmd(lists:append("erlc ", FileName)),

  print:line("Compiled result: ", CompileResult),

  {ok,CompileResult};

%% Operating System Command
module_or_expression([$;,$;|Code], _Bindings) ->
  OsCmd1 = string:concat("os:cmd(\"", Code),
  OsCmd2 = string:concat(OsCmd1, "\")."),
  execute(OsCmd2, _Bindings);

%% Erlang Expression
module_or_expression(Code, Bindings) ->
  execute(Code, Bindings).

%% @spec get_module_name_from_code(list()) -> list()
%% @doc Parses the module name from the module code
get_module_name_from_code([$)|_]) ->
  "";
get_module_name_from_code([$_|RestOfCode]) ->
  lists:append([$_], get_module_name_from_code(RestOfCode));
get_module_name_from_code([Char|RestOfCode]) when (Char > 96) and (Char < 123) ->
  lists:append([Char], get_module_name_from_code(RestOfCode)).

%% @doc Executes the given code with the given variable bindings
%%      Returns the code execution value and the new variable bindings
execute(Code, Bindings) ->
  try
    {ok, Tokens, _} = erl_scan:string(Code),
    {ok, [Form]}    = erl_parse:parse_exprs(Tokens),

    {value, Value, NewBindings} = erl_eval:expr(Form, Bindings),

    % Convert Value to something printable.
    % This is required in order to display the result in the "Out[x]"` field.
    ReturnValue = list_to_binary(lists:flatten(io_lib:format("~p", [Value]))),

    {ok, ReturnValue, NewBindings}
  catch
    Exception:Reason ->
      E = lists:flatten(io_lib:format("~p", [Exception])),
      R = lists:flatten(io_lib:format("~p", [Reason])),

      {error, E, R}
  end.
