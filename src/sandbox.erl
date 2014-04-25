%%%-------------------------------------------------------------------
%%% @author Roberto Aloi
%%% @doc Erlang Sanbox to run erlang code in a restricted environment
%%% @end
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% THE FOLLOWING MODULE WAS WRITTEN BY ROBERT ALOI, WHO MADE HIS ERLANG
%%% SANDBOX CODE AVAILABLE ON GITHUB @ https://github.com/robertoaloi/erlang-sandbox
%%%-------------------------------------------------------------------

-module(sandbox).

-export([eval/1, eval/2]).

-export([restricted_msg/0]).

-define(MAX_HEAP_SIZE, 10000).
-define(MAX_ARGS_SIZE, 200).
-define(MAX_SIZE_QUALIFIER_DIMENSION, 500).

-define(ATOM_PREFIX, "axwlefhubay_").

eval(E) ->
  eval(E, []).

eval(E, Bs) ->
  {ok, Tokens, _} = erl_scan:string(E),
  {ok, Exprs} = erl_parse:parse_exprs(Tokens),
  SafeExprs = safe_exprs(Exprs),
  {value, Value, NBs} = erl_eval:exprs(SafeExprs, Bs, {eval, fun lh/3}, {value, fun nlh/2}),
  {erl_syntax:concrete(restore_exprs(erl_syntax:abstract(Value))),
    erl_syntax:concrete(restore_exprs(erl_syntax:abstract(NBs)))}.

lh(f, [], _Bs) ->
  {value, ok, erl_eval:new_bindings()};
lh(f, [{var,_,Name}], Bs) ->
  {value, ok, erl_eval:del_binding(Name, Bs)};
lh(F, Args, Bs) ->
  Arity = length(Args),
  case erlang:function_exported(user_default, F, Arity) of
    true ->
      {eval, erlang:make_fun(user_default, F, Arity), Args, Bs};
    false ->
      {value, sandbox:restricted_msg(), Bs}
  end.

nlh({M, F}, Args) ->
  apply(M, F, Args);
nlh(_F, _Args) ->
  sandbox:restricted_msg().

safe_application(Node) ->
  case erl_syntax:type(Node) of
    application ->
      case erl_syntax_lib:analyze_application(Node) of
        {FakeModule, {FakeFunction, _Arity}} ->
            ?ATOM_PREFIX ++ RealModule = atom_to_list(FakeModule),
          Module = list_to_atom(RealModule),
            ?ATOM_PREFIX ++ RealFunction = atom_to_list(FakeFunction),
          Function = list_to_atom(RealFunction),
          Args = erl_syntax:application_arguments(Node),
          case restrictions:is_allowed(Module, Function, Args) of
            {true, Mock} ->
              erl_syntax:application(
                erl_syntax:atom(Mock),
                erl_syntax:atom(Function),
                Args);
            true ->
              erl_syntax:application(
                erl_syntax:atom(Module),
                erl_syntax:atom(Function),
                Args);
            false ->
              sandbox:restricted_msg()
          end;
        {FakeFunction, _Arity} ->
            ?ATOM_PREFIX ++ RealFunction = atom_to_list(FakeFunction),
          Function = list_to_atom(RealFunction),
          Args = erl_syntax:application_arguments(Node),
          case restrictions:is_allowed(Function, Args) of
            {true, Mock} ->
              erl_syntax:application(
                erl_syntax:atom(Mock),
                erl_syntax:atom(Function),
                Args);
            true ->
              erl_syntax:application(
                erl_syntax:atom(Function),
                Args);
            false ->
              sandbox:restricted_msg()
          end;
        _Arity ->
          sandbox:restricted_msg()
      end;
    fun_expr ->
      sandbox:restricted_msg();
    size_qualifier ->
      SizeQualifier = erl_syntax:size_qualifier_argument(Node),
      case SizeQualifier of
        {integer, 1, Value} when Value < ?MAX_SIZE_QUALIFIER_DIMENSION ->
          Node;
        _ ->
          sandbox:restricted_msg()
      end;
    list_comp ->
      ListCompBody = erl_syntax:list_comp_body(Node),
      case length(ListCompBody) =< 1 of
        true ->
          Node;
        false ->
          sandbox:restricted_msg()
      end;
    binary_comp ->
      BinaryCompBody = erl_syntax:binary_comp_body(Node),
      case length(BinaryCompBody) =< 1 of
        true ->
          Node;
        false ->
          sandbox:restricted_msg()
      end;
    _Else ->
      Node
  end.

replace_atoms(Node) ->
  case erl_syntax:type(Node) of
    atom ->
      AtomName = erl_syntax:atom_name(Node),
      case AtomName of
        "true" ->
          Node;
        "false" ->
          Node;
        _ ->
          erl_syntax:atom(list_to_atom(?ATOM_PREFIX ++ AtomName))
      end;
    _ ->
      Node
  end.

restore_atoms(Node) ->
  case erl_syntax:type(Node) of
    atom ->
      case erl_syntax:atom_name(Node) of
        ?ATOM_PREFIX ++ Atom ->
          erl_syntax:atom(list_to_atom(Atom));
        Else ->
          erl_syntax:atom(list_to_atom(Else))
      end;
    _ ->
      Node
  end.

safe_exprs(Exprs) ->
  revert(safe_expr(Exprs)).

restore_exprs(Exprs) ->
  revert(restore_expr(Exprs)).

revert(Tree) when is_list(Tree) ->
  [erl_syntax:revert(T) || T <- lists:flatten(Tree)];
revert(Tree) ->
  erl_syntax:revert(Tree).

safe_expr(Exprs) when is_list(Exprs) ->
  [safe_expr(Expr) || Expr <- Exprs];
safe_expr(Expr) ->
  postorder(fun safe_application/1,
    postorder(fun replace_atoms/1, Expr)).

restore_expr(Exprs) when is_list(Exprs) ->
  [restore_expr(Expr) || Expr <- Exprs];
restore_expr(Expr) ->
  postorder(fun restore_atoms/1, Expr).

postorder(F, Tree) ->
  F(case erl_syntax:subtrees(Tree) of
      [] ->
        Tree;
      List ->
        erl_syntax:update_tree(Tree,
          [[postorder(F, Subtree)
            || Subtree <- Group]
            || Group <- List])
    end).

restricted_msg() ->
  erlang:error(restricted).