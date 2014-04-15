-module (code_manager).
-export ([module_or_expression/1]).

%% @spec module_or_expression(list()) -> tuple()
%% @doc Determines whether the incoming code is a module that is to be compiled
%%      or an erlang expression to be executed.
module_or_expression([$-,$m,$o,$d,$u,$l,$e,$(|RestOfCode])->
  erlang:display("In module or expression"),
  % If code is a module, do the following steps:
  % 1. Get module name.
  % 2. Save code to file
  % 3. Compile in [Some folder name]
  % 4. Capture and return compilation output

  % 1. Get module name
  ModuleName = get_module_name_from_code(RestOfCode),
  erlang:display(ModuleName),

  % 2. Save file in some folder TODO - Make separate folder for modules
  FullCode = lists:append("-module(", RestOfCode),
  FileName = lists:append(ModuleName, ".erl"),
  {ok, IODevice} = file:open(FileName, [write]), file:write(IODevice, FullCode), file:close(IODevice),

  erlang:display("Trying to compile module"),
  % 3 + 4. Compile Module, Capture Result and Return
  CompileResult = os:cmd(lists:append("erlc ", FileName)),
  erlang:display("Trying to compile module"),
  erlang:display("Type of compileresult = "),
  erlang:display(type_of(CompileResult)),
  {ok,CompileResult};
module_or_expression([$;,$;|Code])->
  OsCmd1 = string:concat("os:cmd(\"", Code),
  OsCmd2 = string:concat(OsCmd1, "\")."),
  execute(OsCmd2);
module_or_expression(Code)->
  execute(Code).


%% @spec get_module_name_from_code(list()) -> list()
%% @doc Parses the module name from the module code
get_module_name_from_code([$)|_])->
  "";
get_module_name_from_code([$_|RestOfCode])->
  lists:append([$_], get_module_name_from_code(RestOfCode));
get_module_name_from_code([Char|RestOfCode]) when (Char > 96) and (Char < 123)->
  lists:append([Char], get_module_name_from_code(RestOfCode)).


execute(Code)->
	try
	    {ok, Tokens, _} = erl_scan:string(Code),
	    {ok, [Form]} = erl_parse:parse_exprs(Tokens),
	    {value, Value, Binding} = erl_eval:expr(Form, []),
      case type_of(Value) of
        %Convert these types to a string due to errors with json encoding
        float -> ReturnValue = float_to_list(Value);
        pid -> ReturnValue = pid_to_list(Value);
        bitstring -> ReturnValue = bitstring_to_list(Value); %TODO - Bitstrings like <<1:1>> not working
        tuple -> ReturnValue = tuple_to_list(Value);
        _Else -> ReturnValue = Value
      end,
      io:format("code execution return value = ~p~n", [ReturnValue]),
	    {ok, ReturnValue, Binding}
	catch
	    Exception:Reason ->
	    {error, Exception, Reason}
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

type_of(_X)                     -> unknown.