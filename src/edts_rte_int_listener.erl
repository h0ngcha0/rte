%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc%%%
%%% This file is part of EDTS.
%%%
%%% EDTS is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% EDTS is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with EDTS. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(edts_rte_int_listener).

-behaviour(gen_server).

%%%_* Exports =================================================================

%% server API
-export([start/0, stop/0, start_link/0]).

-export([ interpret_modules/1
        , interpret_node/1
        , is_node_interpreted/0
        , is_module_interpreted/1
        , maybe_attach/1
        , set_breakpoint/3
        , step/0
        , uninterpret_modules/1
        , uninterpret_node/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================
-define(SERVER, ?MODULE).

-record(listener_state, { listener = undefined   :: undefined | pid()
                        , proc = unattached      :: unattached | pid()
                        , subscribers = []         :: [term()]
                        , interpretation = false :: boolean()
                        }).

%%%_* Types ====================================================================
-type state():: #listener_state{}.

%%%_* API ======================================================================
start() ->
  ?MODULE:start_link(),
  {node(), ok}.

stop() ->
  ok.

%%------------------------------------------------------------------------------
%% @doc
%% Potentially attach to an interpreter process Pid. Will not
%% reattach if already attached.
%% @end
-spec maybe_attach(Pid :: pid()) -> {attached, pid(), pid()}
                                  | {already_attached, pid(), pid()}.
%%------------------------------------------------------------------------------
maybe_attach(Pid) ->
  edts_rte_app:debug("in maybe_attach, Pid:~p~n", [Pid]),
  case gen_server:call(?SERVER, {attach, Pid}) of
    {ok, attach, AttPid} ->
      {attached, AttPid, Pid};
    {error, already_attached, AttPid} ->
      {already_attached, AttPid, Pid}
  end.

%%------------------------------------------------------------------------------
%% @doc
%% Make Modules interpretable. Returns the list of modules which were
%% interpretable and set as such.
%% @end
-spec interpret_modules(Modules :: [module()]) -> {ok, [module()]}.
%%------------------------------------------------------------------------------
interpret_modules(Modules) ->
  gen_server:call(?SERVER, {interpret, Modules}).

%%------------------------------------------------------------------------------
%% @doc
%% Make all loaded modules interpretable.
%% Returns the list of modules which were
%% interpretable and set as such.
%% @end
-spec interpret_node(Exclusions :: [module()]) -> {ok, [module()]}.
%%------------------------------------------------------------------------------
interpret_node(Exclusions) ->
  interpret_modules(get_safe_modules(Exclusions)).

%%------------------------------------------------------------------------------
%% @doc
%% Reports if code in the current project node is being interpreted.
%% @end
-spec is_node_interpreted() -> boolean().
%%------------------------------------------------------------------------------
is_node_interpreted() ->
  gen_server:call(?SERVER, is_node_interpreted).

%%------------------------------------------------------------------------------
%% @doc
%% Reports if Module is interpreted.
%% @end
-spec is_module_interpreted(Module :: module()) -> boolean().
%%------------------------------------------------------------------------------
is_module_interpreted(Module) ->
  gen_server:call(?SERVER, {is_interpreted, Module}).

%%------------------------------------------------------------------------------
%% @doc
%% Toggles a breakpoint at Module:Line.
%% @end
-spec set_breakpoint(Module :: module(), Fun :: function(), Arity :: non_neg_integer()) ->
                        {error, function_not_found} | {ok, set, tuple()}.

%%------------------------------------------------------------------------------
set_breakpoint(Module, Fun, Arity) ->
  gen_server:call(?SERVER, {set_breakpoint, Module, Fun, Arity}).

%%------------------------------------------------------------------------------
%% @doc
%% Uninterprets Modules.
%% @end
-spec uninterpret_modules(Modules :: [module()]) -> ok.
%%------------------------------------------------------------------------------
uninterpret_modules(Modules) ->
  gen_server:call(?SERVER, {uninterpret, Modules}).

%%------------------------------------------------------------------------------
%% @doc
%% Make all interpreted modules uninterpretable.
%% @end
-spec uninterpret_node() -> {ok, [module()]}.
%%------------------------------------------------------------------------------
uninterpret_node() ->
  uninterpret_modules(int:interpreted()).

%%------------------------------------------------------------------------------
%% @doc
%% Orders the interpreter to step in execution.
%% @end
-spec step() -> ok.
%%------------------------------------------------------------------------------
step() ->
  gen_server:call(?SERVER, step, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
%%-----------------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%_* gen_server callbacks  ====================================================
%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
-spec init(list()) -> {ok, state()} |
                      {ok, state(), timeout()} |
                      ignore |
                      {stop, atom()}.
%%------------------------------------------------------------------------------
init([]) ->
  int:auto_attach([break], {?MODULE, maybe_attach, []}),
  {ok, #listener_state{}}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%
-spec handle_call(term(), {pid(), atom()}, state()) ->
                     {reply, Reply::term(), state()} |
                     {reply, Reply::term(), state(), timeout()} |
                     {noreply, state()} |
                     {noreply, state(), timeout()} |
                     {stop, Reason::atom(), term(), state()} |
                     {stop, Reason::atom(), state()}.
%%------------------------------------------------------------------------------
handle_call({attach, Pid}, _From, #listener_state{proc = unattached} = State) ->
  ok = do_attach_pid(Pid),
  %% step thru...
  edts_rte_server:finished_attach(Pid),
  {reply, {ok, attach, self()}, State#listener_state{proc = Pid}};
handle_call( {attach, Pid}, _From
           , #listener_state{listener = Listener, proc = Pid} = State) ->
  edts_rte_app:debug("in hancle_call, already attach, Pid:~p~n", [Pid]),
  {reply, {error, {already_attached, Listener, Pid}}, State};

handle_call({interpret, Modules}, _From, State) ->
  Reply = lists:filter(fun(E) -> E =/= mod_uninterpretable end,
                       lists:map(fun(Module) ->
                                     try
                                       case int:interpretable(Module) of
                                         true -> {module, Name} = int:i(Module),
                                                 Name;
                                         _    -> mod_uninterpretable
                                       end
                                     catch
                                       _:_ -> mod_uninterpretable
                                     end
                                 end, Modules)),
  {reply, Reply, State#listener_state{interpretation = true}};

handle_call({set_breakpoint, Module, Fun, Arity}, _From, State) ->
  Reply = case int:break_in(Module, Fun, Arity) of
            ok    -> {ok, set, {Module, Fun, Arity}};
            Error -> edts_rte_app:debug("set_breakpoint error:~p~n", [Error]),
                     Error
          end,
  {reply, Reply, State};

handle_call({uninterpret, Modules}, _From, State) ->
  lists:map(fun(Module) -> int:n(Module) end, Modules),
  {reply, {ok, uninterpreted}, State#listener_state{interpretation = false}};

handle_call({is_interpreted, Module}, _From, State) ->
  {reply, lists:member(Module, int:interpreted()), State};

handle_call(is_node_interpreted, _From,
            #listener_state{interpretation = Value} = State) ->
  {reply, Value, State};

handle_call(_Cmd, _From, #listener_state{proc = unattached} = State) ->
  {reply, {error, unattached}, State};

handle_call(continue, From, #listener_state{proc = Pid} = State) ->
  edts_rte_app:debug("before int:continue~n"),
  int:continue(Pid),
  edts_rte_app:debug("after int:continue. pid~p~n", [Pid]),
  Subs = State#listener_state.subscribers,
  {noreply, State#listener_state{subscribers = add_to_ulist(From, Subs)}};

handle_call(step, From, #listener_state{proc = Pid} = State) ->
  int:step(Pid),
  Subs = State#listener_state.subscribers,
  {noreply, State#listener_state{subscribers = add_to_ulist(From, Subs)}};

handle_call(step_out, From, #listener_state{proc = Pid} = State) ->
  int:finish(Pid),
  Subs = State#listener_state.subscribers,
  {noreply, State#listener_state{subscribers = add_to_ulist(From, Subs)}}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
-spec handle_cast(Msg::term(), state()) -> {noreply, state()} |
                                           {noreply, state(), timeout()} |
                                           {stop, Reason::atom(), state()}.
%%------------------------------------------------------------------------------
handle_cast({register_attached, Pid}, State) ->
  {noreply, State#listener_state{listener = Pid}};
handle_cast({notify, Info}, #listener_state{subscribers = Subs} = State) ->
  notify(Info, Subs),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%% @end
%%
-spec handle_info(term(), state()) -> {noreply, state()} |
                                      {noreply, state(), Timeout::timeout()} |
                                      {stop, Reason::atom(), state()}.
%%------------------------------------------------------------------------------
%% Hit a breakpoint
handle_info({Meta, {break_at, Module, Line, Depth}}, State) ->
  Bindings = int:meta(Meta, bindings, nostack),
  File = int:file(Module),
  notify({break, File, {Module, Line}, Depth, Bindings}),
  edts_rte_server:break_at({Bindings, Module, Line, Depth}),
  {noreply, State};

%% Became idle (not executing any code under debugging)
handle_info({_Meta, idle}, State) ->
  edts_rte_app:debug("in handle_info, idle~n"),
  %% Crap, why this can't be executed?
  %% Bindings = int:meta(Meta, bindings, nostack),
  notify(idle),
  {noreply, State};

%% Came back from uninterpreted code
handle_info({_Meta, {re_entry, _, _}}, State) ->
  {noreply, State};

%% Running code, but not telling anything really relevant
handle_info({_Meta, running}, State) ->
  {noreply, State};

%% Something attached to the debugger (most likely ourselves)
handle_info({_Meta, {attached, _, _, _}}, State) ->
  {noreply, State};

%% Process under debug terminated
handle_info({Meta, {exit_at, _, _Reason, _}}, State) ->
  Bindings = int:meta(Meta, bindings, nostack),
  edts_rte_app:debug("in handle_info, till exit_at, Bindings:~p~n", [Bindings]),
  edts_rte_server:send_exit(),
  edts_rte_app:debug("exit signal sent~n"),
  {noreply, State#listener_state{proc = unattached}};

handle_info(Msg, State) ->
  error_logger:info_msg("Unexpected message: ~p~n", [Msg]),
  {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
-spec terminate(Reason::atom(), state()) -> any().
%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
  int:auto_attach(false),
  ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
-spec code_change(OldVsn::string(), state(), Extra::term()) -> {ok, state()}.
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_attach_pid(Pid) ->
  edts_rte_app:debug("in handle_call, attach, Pid:~p~n", [Pid]),
  register_attached(self()),
  int:attached(Pid),
  edts_rte_app:debug("rte listener ~p attached to ~p~n", [self(), Pid]),
  ok.

%%------------------------------------------------------------------------------
%% @doc
%% Notifies all registered rte listener clients of a change in debugger state
%% through Info
%%
-spec notify(Info :: term()) -> ok.
%%------------------------------------------------------------------------------
notify(Info) ->
  gen_server:cast(?SERVER, {notify, Info}).

notify(_, []) ->
  ok;
notify(Info, [Client|R]) ->
  gen_server:reply(Client, {ok, Info}),
  notify(Info, R).

%%------------------------------------------------------------------------------
%% @doc
%% Register in idbg_server as a debugger process attached to Pid.
%%
-spec register_attached(Pid :: pid()) -> ok.
%%------------------------------------------------------------------------------
register_attached(Pid) ->
  gen_server:cast(?SERVER, {register_attached, Pid}).

%%------------------------------------------------------------------------------
%% @doc
%% Returns a list of all loaded modules except OTP modules and those
%% explicitly belonging to ExcludedApps
%%
-spec get_safe_modules(ExcludedApps :: [atom()]) -> [module()].
%%------------------------------------------------------------------------------
get_safe_modules(ExcludedApps) ->
  ExcludedAppDirs = [code:lib_dir(App, ebin) || App <- ExcludedApps],
  ErlLibDir = code:lib_dir(),
  [Module || {Module, Filename} <- code:all_loaded(),
             is_list(Filename),
             not lists:prefix(ErlLibDir, Filename),
             not code:is_module_native(Module),
             not lists:member(filename:dirname(Filename), ExcludedAppDirs)].

add_to_ulist(E, L) ->
  case lists:member(E, L) of
    true  -> L;
    false -> [E|L]
  end.

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
