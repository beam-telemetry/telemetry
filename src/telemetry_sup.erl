%%%-------------------------------------------------------------------
%% @doc telemetry top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(telemetry_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    TableHandler = #{id => telemetry_table_handler,
                     start => {telemetry_table_handler, start_link, []},
                     restart => permanent,
                     shutdown => 5000,
                     type => worker,
                     modules => [telemetry_table_handler]},
    {ok, {SupFlags, [TableHandler]}}.
