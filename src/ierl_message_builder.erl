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

-export([
  generate_content/2,
  generate_header/3
]).

%%====================================================================
%% API functions
%%====================================================================

%% @spec generate_header(list(), list(), list()) -> list()
%% @doc Creates the header for the message being sent to IPython
generate_header(Session, MessageType, Date) ->
  HeaderPropList = {struct, [
    {date,     Date                       },
    {username, "ierlang_kernel"           },
    {session,  Session                    },
    {msg_id,   uuid:to_string(uuid:v4())  },
    {msg_type, list_to_binary(MessageType)}
  ]},

  mochijson2:encode(HeaderPropList).

generate_content(Type, Args) ->
  Content = case Type of
    busy                -> {struct, [{execution_state, <<"busy">>    }]};
    idle                -> {struct, [{execution_state, <<"idle">>    }]};
    starting            -> {struct, [{execution_state, <<"starting">>}]};
    metadata            -> {struct, []};
    kernel_info_reply   -> kernel_info_reply();
    execute_reply       -> execute_reply(Args);
    execute_reply_error -> execute_reply_error(Args);
    pyout               -> pyout(Args);
    pyin                -> pyin(Args);
    pyerr               -> pyerr(Args);
    display_data        -> display_data(Args)
  end,

  mochijson2:encode(Content).

%%====================================================================
%% Internal functions
%%====================================================================

kernel_info_reply() ->
  {struct, [
    {protocol_version, [4, 1]              },
    {language_version, [0, 0, 1]           },
    {ipython_version,  [2, 0, 0, <<"dev">>]},
    {language,         <<"erlang">>        }
  ]}.

execute_reply({"ok", ExecutionCount, _UserVars, _UserExprs}) ->
  {struct, [
    {status,           <<"ok">>},
    {execution_count,  ExecutionCount},
    {payload,          []},
    {user_variables,   {}},
    {user_expressions, {}}
  ]}.

execute_reply_error({"error", ExecutionCount, ExceptionName, _ExceptionValue, Traceback}) ->
  {struct, [
    {status,          <<"error">>},
    {execution_count, ExecutionCount},
    {ename,           ExceptionName},
    {evalue,          <<"ERROR">>},
    {traceback,       Traceback}
  ]}.

pyout({ExecutionCount, CodeOutput}) ->
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
          {'text/html',  FrmtCode},
          {'text/plain', FrmtCode}
        ]},
        FrmtDataJson = mochijson2:encode(FrmtData),

        FrmtContent = {struct,    [
        {execution_count, ExecutionCount},
        {data,            FrmtDataJson},
        {metadata,        {}}
        ]},

        mochijson2:encode(FrmtContent)
  end,
  PyoutContent.

pyin({Code, ExecutionCount}) ->
  {struct, [
    {execution_count, ExecutionCount},
    {code,            Code}
  ]}.

pyerr({_ExceptionName, ExecutionCount, _ExceptionValue, Traceback}) ->
  {struct, [
    {execution_count, ExecutionCount},
    {ename,           <<"error">>       },
    {evalue,          <<"ERROR">>       },
    {traceback,       Traceback     }
  ]}.

display_data({Source, RawData, _MetaData}) ->
  DataStruct = {struct, [
    {'text/html',  RawData},
    {'text/plain', RawData}
  ]},

  Data = mochijson2:encode(DataStruct),

  % Create the content
  Content = {struct,    [
    {source,   Source},
    {data,     Data},
    {metadata, {}}
  ]}.
