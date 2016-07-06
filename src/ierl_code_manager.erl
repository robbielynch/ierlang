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
-define(RESTRICTED_MODE, false).

%% @spec module_or_expression(list(), list()) -> tuple()
%% @doc Determines whether the incoming code is a module that is to be compiled
%%      or an erlang expression to be executed.
module_or_expression([$-,$m,$o,$d,$u,$l,$e,$(|RestOfCode], _Bindings)->
  case ?RESTRICTED_MODE of
    true -> {ok, "Restricted"};
    _    ->
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
      print:line("Trying to compile module"),
      print:line("Type of compileresult = "),
      print:line(type_of(CompileResult)),
      {ok,CompileResult}
  end;

%% Operating System Command
module_or_expression([$;,$;|Code], _Bindings)->
  case ?RESTRICTED_MODE of
    true -> {ok, "Restricted"};
    _Else ->
      OsCmd1 = string:concat("os:cmd(\"", Code),
      OsCmd2 = string:concat(OsCmd1, "\")."),
      execute(OsCmd2, _Bindings)
  end;

%% Erlang Expression
module_or_expression(Code, Bindings)->
  execute(Code, Bindings).


%% @spec get_module_name_from_code(list()) -> list()
%% @doc Parses the module name from the module code
get_module_name_from_code([$)|_])->
  "";
get_module_name_from_code([$_|RestOfCode])->
  lists:append([$_], get_module_name_from_code(RestOfCode));
get_module_name_from_code([Char|RestOfCode]) when (Char > 96) and (Char < 123)->
  lists:append([Char], get_module_name_from_code(RestOfCode)).

%% @doc Executes the given code with the given variable bindings
%%      Returns the code execution value and the new variable bindings
execute(Code, Bindings)->
  case ?RESTRICTED_MODE of
    %%% If restricted mode is enabled, use the Erlang Sandbox
    true ->
      try
        {Value, NewBindings} = sandbox:eval(Code, Bindings),
        case type_of(Value) of
          list -> ReturnValue = Value;
          integer -> ReturnValue = Value;
          float -> ReturnValue = Value;
          _ -> ReturnValue = lists:flatten(io_lib:format("~p", [Value]))
        end,
        {ok, ReturnValue, NewBindings}
      catch
        Exception:Reason ->
          E = lists:flatten(io_lib:format("~p", [Exception])),
          R = lists:flatten(io_lib:format("~p", [Reason])),
          print:line("Exception = ", [E]),
          print:line("Reason = ", [R]),
          {error, E, R}
      end;
    _ ->
      %%% If restricted mode is disabled, evaluate any expression
      try
        {ok, Tokens, _} = erl_scan:string(Code),
        {ok, [Form]} = erl_parse:parse_exprs(Tokens),
        {value, Value, NewBindings} = erl_eval:expr(Form, Bindings),
        % Convert Value to something printable.
        % This is required in order to allow all data structures
        % to be encoded to json
        case type_of(Value) of
          list -> ReturnValue = Value;
          integer -> ReturnValue = Value;
          float -> ReturnValue = Value;
          _ -> ReturnValue = lists:flatten(io_lib:format("~p", [Value]))
        end,
        print:line("code execution return value = ", [ReturnValue]),
        {ok, ReturnValue, NewBindings}
      catch
        Exception:Reason ->
          E = lists:flatten(io_lib:format("~p", [Exception])),
          R = lists:flatten(io_lib:format("~p", [Reason])),
          print:line("Exception = ", [E]),
          print:line("Reason = ", [R]),
          {error, E, R}
      end
  end.


type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
type_of(X) when is_bitstring(X) -> bitstring;  % will fail before e12
type_of(X) when is_binary(X)    -> binary;
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;
type_of(_)                      -> unknown.
