-module(peer2peer_ws_h).

%%% ==================================================================
%%% API Function Exports
%%% ==================================================================

-export([
  init/2,
  websocket_init/1,
  websocket_handle/2,
  websocket_info/2,
  websocket_terminate/2
]).

%%% ==================================================================
%%% API Function Definitions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Init
%% @end
%% -------------------------------------------------------------------
init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

%% -------------------------------------------------------------------
%% @doc
%% Websocket Init
%% @end
%% -------------------------------------------------------------------
websocket_init(State) ->
	{ok, State, hibernate}.

%% -------------------------------------------------------------------
%% @doc
%% Websocket Handle
%% @end
%% -------------------------------------------------------------------
websocket_handle({text, Data}, State) ->
	JsonTerm = jiffy:decode(Data, [return_maps]),
	Type = maps:get(<<"type">>, JsonTerm, undefined),
	case Type of
		<<"GETROOM">> ->
			Room = rand:uniform(999999),
			DataRoom = jiffy:encode(#{<<"type">> => <<"GETROOM">>, <<"value">> => Room}),
			gproc:reg({p,l, Room}),
			{reply, {text, <<DataRoom/binary>>}, [#{room => Room}], hibernate};
		<<"ENTERROOM">> ->
			Room = maps:get(<<"value">>, JsonTerm, undefined),
			Participants = gproc:lookup_pids({p, l, Room}),
			case length(Participants) of
				1 ->
					gproc:reg({p, l, Room}),
					{ok, [#{room => Room}], hibernate};
				_ ->
					DataRoom = jiffy:encode(#{<<"type">> => <<"WRONGROOM">>}),
					{reply, {text, <<DataRoom/binary>>}, State, hibernate}
			end;
		_ ->
			reply2peer(Data, State),
			{ok, State, hibernate}
	end;

websocket_handle(_, State) ->
	{ok, State, hibernate}.

%% -------------------------------------------------------------------
%% @doc
%% Websocket Info
%% @end
%% -------------------------------------------------------------------
websocket_info(_Info, State) ->
	{reply, {text, _Info}, State, hibernate}.

%% -------------------------------------------------------------------
%% @doc
%% Websocket Terminate
%% @end
%% -------------------------------------------------------------------
websocket_terminate(_, _) ->
	ok.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Reply data to peer
%% @end
%% -------------------------------------------------------------------
reply2peer(Data, [#{room := Room}]) ->
	[Pid ! <<Data/binary>> || Pid <- gproc:lookup_pids({p, l, Room}) -- [self()]].
