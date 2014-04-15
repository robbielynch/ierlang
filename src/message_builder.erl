%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2014 10:51
%%%-------------------------------------------------------------------
-module(message_builder).
-author("Robbie Lynch").

%% API
-export([generate_content_reply/2, generate_content_reply/1,
         msg_parts_to_ipython_msg/5, create_metadata/0,
        generate_header_reply/3]).

%% @spec msg_parts_to_list_of_binary_msg_parts(list(), list(), binary(), list(), list()) -> list()
%% @doc Function to convert all message parts to binary before and place them
%%      in a list before it is sent to IPython via zmq.
msg_parts_to_ipython_msg(Session, KernelHeader, ParentHeader, Content, _Metadata)->
  [
    list_to_binary(Session),									%uuid
    list_to_binary("<IDS|MSG>"),							%delim
    list_to_binary(""),										    %hmac
    list_to_binary(KernelHeader), 						%serialised header
    ParentHeader,											        %serialised parent header
    list_to_binary("{}"),									    %serialised metadata
    list_to_binary(Content)									  %serialised content
  ].

%% @spec generate_header_reply(list(), list(), list()) -> list()
%% @doc Creates the header for the message being sent to IPython
generate_header_reply(Session, MessageType, Date)->
  HeaderPropList = {struct,	[
    {date, Date},
    {username, "ierlang_kernel"},
    {session, Session},
    {msg_id, uuid:to_string(uuid:v4())},
    {msg_type, MessageType}
  ]},
  Header = mochijson2:encode(HeaderPropList),
  Header.


%% @spec generate_content_reply(atom()) -> list()
%% @doc Creates the content reply for the busy status sent over the
%%      iopub socket.
generate_content_reply(busy)->
  %%Should be sent before the execution of the code
  Content = {struct,	[ 	{execution_state, "busy"}	]},
  ContentJson = mochijson2:encode(Content),
  ContentJson;

%% @spec generate_content_reply(atom()) -> list()
%% @doc Creates the content reply for the idle status sent over the
%%      iopub socket.
generate_content_reply(idle)->
  Content = {struct,	[ 	{execution_state, "idle"}	]},
  ContentJson = mochijson2:encode(Content),
  ContentJson;

%% @spec generate_content_reply(atom()) -> list()
%% @doc Creates the content reply for the starting status sent over the
%%      iopub socket.
generate_content_reply(starting)->
  Content = {struct,	[ 	{execution_state, "starting"}	]},
  ContentJson = mochijson2:encode(Content),
  ContentJson;

%% @spec generate_content_reply(atom()) -> list()
%% @doc Creates the content reply for the kernel_info_reply sent over the
%%      shell socket.
generate_content_reply(kernel_info_reply)->
%	Version of messaging protocol (mandatory).
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
  IPythonVersion = [2, 0, 0, "dev"],
%   Language version number (mandatory).
%   It is Python version number (e.g., [2, 7, 3]) for the kernel
%   included in IPython.
%   'language_version': [int, ...],
  LanguageVersion = [0, 0, 1],
%   Programming language in which kernel is implemented (mandatory).
%   Kernel included in IPython returns 'python'.
%   'language': str,
  Language = "erlang",

%	Build the proplist to be converted to json
  Content = {struct,	[
    {protocol_version, ProtocolVersion},
    {language_version, LanguageVersion},
    {ipython_version, IPythonVersion},
    {language, Language}
  ]},
%	Build the Json Reply
  ReplyJson = mochijson2:encode(Content),
  ReplyJson.


%% @doc Creates the content reply for a successful execute_reply
%%      sent over the shell socket.
generate_content_reply(execute_reply, {"ok", ExecutionCount, _UserVars, _UserExprs})->
  %%io:format("payload = ~s~n", [Payload]),
  Content = {struct,	[
    {status, "ok"},
    {execution_count, ExecutionCount},
    {payload, []},
    {user_variables, {}},
    {user_expressions, {}}
  ]},
  ContentJson = mochijson2:encode(Content),
  ContentJson;

%% @spec generate_content_reply(atom(), tuple()) -> list()
%% @doc Creates the content reply for an unsuccessful execute_reply
%%      sent over the shell socket.
generate_content_reply(execute_reply_error, {"error", ExecutionCount, ExceptionName, _ExceptionValue, _Traceback})->
  %%io:format("payload = ~s~n", [Payload]),
  erlang:display("in generate_content_reply for execute reply error"),
  Content = {struct,	[
    {status, "error"},
    {execution_count, ExecutionCount},
    {ename, ExceptionName},
    {evalue, "ERROR DUDE"},
    {traceback, []}
  ]},
  erlang:display("converting to json"),
  ContentJson = mochijson2:encode(Content),
  ContentJson;

%% @spec generate_content_reply(atom(), tuple()) -> list()
%% @doc Creates the content reply for pyout
%%      sent over the iopub socket.
generate_content_reply(pyout, {ExecutionCount, CodeOutput})->
  Data = {struct, [
    {'text/html', CodeOutput},
    {'text/plain', CodeOutput}
  ]},
  DataJson = mochijson2:encode(Data),

  Content = {struct,	[
    {execution_count, ExecutionCount},
    {data, DataJson},
    {metadata, {}}
  ]},
  PyoutContent = mochijson2:encode(Content),
  PyoutContent;

%% @spec generate_content_reply(atom(), tuple()) -> list()
%% @doc Creates the content reply for pyin
%%      sent over the iopub socket.
generate_content_reply(pyin, {Code, ExecutionCount})->
  Content = {struct,	[ 	{execution_count, ExecutionCount},
    {code, Code}
  ]},
  PyinContent = mochijson2:encode(Content),
  PyinContent;

%% @spec generate_content_reply(atom(), tuple()) -> list()
%% @doc Creates the content reply for pyerr
%%      sent over the iopub socket.
generate_content_reply(pyerr, {_ExceptionName, ExecutionCount, _ExceptionValue, _Traceback})->
  Content = {struct,	[
    {execution_count, ExecutionCount},
    {ename, "error dude"},
    {evalue, "ERROR DUDE"},
    {traceback, []}
  ]},
  PyerrContent = mochijson2:encode(Content),
  PyerrContent;

%% @spec generate_content_reply(atom(), tuple()) -> list()
%% @doc Creates the content reply for display_data
%%      sent over the iopub socket.
generate_content_reply(display_data, {Source, Data, MetaData})->
  Content = {struct,	[
    {source, Source},
    {data, Data},
    {metadata, MetaData}
  ]},
  DisplayContent = mochijson2:encode(Content),
  DisplayContent.

%% @spec create_metadata() -> list()
%% @doc Creates the metadata for the outgoing message
create_metadata()->
  Metadata = {struct,	[]},
  Md = mochijson2:encode(Metadata),
  Md.
