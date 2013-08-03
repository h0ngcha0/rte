%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
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

%% This module is the starting point of edts_rte. Its sole purpose is to
%% start the edts_rte supervisor.

%%%_* Module declaration =======================================================
-module(edts_rte_app).

-behaviour(gen_server).

%%%_* Exports =================================================================
%% server API
-export([start/0, stop/0, start_link/0]).

-export([started_p/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([ debug/1
        , debug/2]).

%%%_* Defines ==================================================================
-define(DEBUG, true).
-define(SERVER, ?MODULE).

%%%_* API ======================================================================
start() ->
  ?MODULE:start_link(),
  ok.

stop() ->
  ok.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

started_p() -> whereis(?SERVER) =/= undefined.

%%%_* gen_server callbacks  ====================================================
init([]) ->
  {ok, _Pid} = edts_rte_sup:start(),
  {ok, node()}.

handle_call(Msg, From, State) ->
  {reply, {ok, From, Msg}, State}.

handle_info(_Msg, _State) ->
  {noreply, _State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

debug(Str) -> debug(Str, []).

-ifdef(DEBUG).
debug(FmtStr, Args) -> io:format(FmtStr, Args).
-else.
debug(_FmtStr, _Args) -> ok.
-endif.

%%%_* Unit tests ===============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
