%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Edts RTE Supervisor
%%% @end
%%% @copyright
%%%
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
-module(edts_rte_sup).

-behaviour(supervisor).

%%%_* Exports ==========================================================
%% API
-export([ start/0
        , start_link/0
        , started_p/0
        ]).

%% Supervisor callbacks
-export([init/1]).

%%%_* Macros ===========================================================
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start() ->
  ?MODULE:start_link().

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

started_p() ->
  whereis(?MODULE) =/= undefined.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
  RteServer = ?CHILD(edts_rte_server, worker),
  {ok, { {one_for_one, 5, 10}, [RteServer]}}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
