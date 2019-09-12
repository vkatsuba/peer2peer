%%%-------------------------------------------------------------------
%% @doc peer2peer public API
%% @end
%%%-------------------------------------------------------------------

-module(peer2peer_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, peer2peer, "templates/index.html"}},
      {"/ws", peer2peer_ws_h, []},
      {"/www/[...]", cowboy_static, {priv_dir, peer2peer, "static"}},
      {'_', peer2peer_404_h, []}
    ]}
  ]),
  PrivDir = code:priv_dir(peer2peer),
  {ok, _} = cowboy:start_tls(https, [
    {port, 8000},
    {cacertfile, PrivDir ++ "/ssl/ca.crt"},
    {certfile, PrivDir ++ "/ssl/dummy.crt"},
    {keyfile, PrivDir ++ "/ssl/dummy.key"}
  ], #{env => #{dispatch => Dispatch}}),
  peer2peer_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
