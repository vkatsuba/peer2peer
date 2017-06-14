-module(peer2peer_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{
			'_', [
				{"/ws", ws_handler, []},
				{"/", cowboy_static, {priv_file, peer2peer, "templates/index.html"}},
				{"/css/[...]", cowboy_static, {priv_dir, peer2peer, "static/css", [{mimetypes, cow_mimetypes, all}]}},
				{"/fonts/[...]", cowboy_static, {priv_dir, peer2peer, "static/fonts", [{mimetypes, cow_mimetypes, all}]}},
				{"/img/[...]", cowboy_static, {priv_dir, peer2peer, "static/img", [{mimetypes, cow_mimetypes, all}]}},
				{"/js/[...]", cowboy_static, {priv_dir, peer2peer, "static/js", [{mimetypes, cow_mimetypes, all}]}},
				{'_', error_404, []}
		]}
	]),
	{ok, _} = cowboy:start_http(websocket, 100, [{ip, {127,0,0,1}}, {port, 8080}], [{env, [{dispatch, Dispatch}]}, {max_keepalive, 50}, {timeout, 500}]),
	peer2peer_sup:start_link().

stop(_State) ->
	ok.
