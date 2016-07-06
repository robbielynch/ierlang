%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc Module that contains the functions to create replies to
%%%      IPython messages.
%%%
%%% @end
%%% Created : 03. Apr 2014 10:51
%%%-------------------------------------------------------------------
-module(ierl_message_builder).
-author("Robbie Lynch").

-define(USERNAME,        "ierlang_kernel").
-define(IDLE_STATUS,     "idle"          ).
-define(BUSY_STATUS,     "busy"          ).
-define(STARTING_STATUS, "starting"      ).
-define(OK_STATUS,       "ok"            ).
-define(ERROR_STATUS,    "error"         ).

-export([
  create_metadata/0,
  generate_content_reply/1,
  generate_content_reply/2,
  generate_header_reply/3
]).

%% @spec generate_header_reply(list(), list(), list()) -> list()
%% @doc Creates the header for the message being sent to IPython
generate_header_reply(Session, MessageType, Date) ->
  HeaderPropList = {struct, [
    {date,     Date                       },
    {username, ?USERNAME                  },
    {session,  Session                    },
    {msg_id,   uuid:to_string(uuid:v4())  },
    {msg_type, list_to_binary(MessageType)}
  ]},

  mochijson2:encode(HeaderPropList).

%% @spec generate_content_reply(atom()) -> list()
%% @doc Creates the content reply for the busy status sent over the
%%      iopub socket.
generate_content_reply(busy)->
  %%Should be sent before the execution of the code
  Content = {struct, [{execution_state, ?BUSY_STATUS}]},

  mochijson2:encode(Content);

generate_content_reply(idle)->
  Content = {struct,    [     {execution_state, ?IDLE_STATUS}    ]},

  mochijson2:encode(Content);

generate_content_reply(starting)->
  Content = {struct,    [     {execution_state, ?STARTING_STATUS}    ]},

  mochijson2:encode(Content);

generate_content_reply(kernel_info_reply)->
%    Version of messaging protocol (mandatory).
%   The first integer indicates major version.  It is incremented when
%   there is any backward incompatible change.
%   The second integer indicates minor version.  It is incremented when
%   there is any backward compatible change.
%   'protocol_version': [int, int],
  ProtocolVersion = [4, 1],
%   IPython version number (optional).
%   Non-python kernel backend may not have this version number.
%   The last component is an extra field, which may be 'dev' or
%   'rc1' in development version.  It is an empty string for
%   released version.
%   'ipython_version': [int, int, int, str],
  IPythonVersion = [2, 0, 0, <<"dev">>],
%   Language version number (mandatory).
%   It is Python version number (e.g., [2, 7, 3]) for the kernel
%   included in IPython.
%   'language_version': [int, ...],
  LanguageVersion = [0, 0, 1],
%   Programming language in which kernel is implemented (mandatory).
%   Kernel included in IPython returns 'python'.
%   'language': str,
  Language = <<"erlang">>,

%    Build the proplist to be converted to json
  Content = {struct,    [
    {protocol_version, ProtocolVersion},
    {language_version, LanguageVersion},
    {ipython_version, IPythonVersion},
    {language, Language}
  ]},
%    Build the Json Reply
  ReplyJson = mochijson2:encode(Content),
  ReplyJson.

generate_content_reply(execute_reply, {"ok", ExecutionCount, _UserVars, _UserExprs})->
  Content = {struct,    [
    {status, ?OK_STATUS},
    {execution_count, ExecutionCount},
    {payload, []},
    {user_variables, {}},
    {user_expressions, {}}
  ]},
  ContentJson = mochijson2:encode(Content),
  ContentJson;

generate_content_reply(execute_reply_error, {"error", ExecutionCount, ExceptionName, _ExceptionValue, Traceback})->
  print:line("in generate_content_reply for execute reply error"),
  Content = {struct,    [
    {status, ?ERROR_STATUS},
    {execution_count, ExecutionCount},
    {ename, ExceptionName},
    {evalue, "ERROR"},
    {traceback, Traceback}
  ]},
  print:line("converting to json"),
  ContentJson = mochijson2:encode(Content),
  ContentJson;

generate_content_reply(pyout, {ExecutionCount, CodeOutput})->
  PyoutContent =
  try
    Data = {struct, [
    {'text/html', CodeOutput},
    {'text/plain', CodeOutput}
  ]},
  DataJson = mochijson2:encode(Data),

  Content = {struct,    [
    {execution_count, ExecutionCount},
    {data, DataJson},
    {metadata, {}}
  ]},
  mochijson2:encode(Content)
  catch
      _:_ ->
        FrmtCode = io_lib:format("~p", [CodeOutput]),
        FrmtData = {struct, [
          {'text/html', FrmtCode},
          {'text/plain', FrmtCode}
        ]},
        FrmtDataJson = mochijson2:encode(FrmtData),

        FrmtContent = {struct,    [
        {execution_count, ExecutionCount},
        {data, FrmtDataJson},
        {metadata, {}}
        ]},
        mochijson2:encode(FrmtContent)
  end,
  PyoutContent;

generate_content_reply(pyin, {Code, ExecutionCount})->
  Content = {struct,    [     {execution_count, ExecutionCount},
    {code, Code}
  ]},
  PyinContent = mochijson2:encode(Content),
  PyinContent;

generate_content_reply(pyerr, {_ExceptionName, ExecutionCount, _ExceptionValue, Traceback})->
  Content = {struct,    [
    {execution_count, ExecutionCount},
    {ename, "error"},
    {evalue, "ERROR"},
    {traceback, Traceback}
  ]},

  mochijson2:encode(Content);

generate_content_reply(display_data, {Source, RawData, _MetaData})->
  % Create the data dictionary with mime types as keys
  DataStruct = {struct, [
    {'text/html', RawData},
    {'text/plain', RawData}
  ]},
  Data = mochijson2:encode(DataStruct),

  % Create the content
  Content = {struct,    [
    {source, Source},
    {data, Data},
    {metadata, {}}
  ]},

  mochijson2:encode(Content).

%% @spec create_metadata() -> list()
%% @doc Creates the metadata for the outgoing message
create_metadata()->
  Metadata = {struct,    []},
  mochijson2:encode(Metadata).
