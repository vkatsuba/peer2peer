-module(ws_handler).

-behaviour(cowboy_websocket_handler).

%%% ==================================================================
%%% API Function Exports
%%% ==================================================================

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%%% ==================================================================
%%% Records
%%% ==================================================================

-record(state, {
	client = undefined :: undefined | binary(),
	room = undefined :: undefined | integer()
}).

%%% ==================================================================
%%% API Function Definitions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Upgrade the protocol to `cowboy_websocket'.
%%
%% @spec init({TransportName, ProtocolName}, Req, Opts) ->  {upgrade, protocol, cowboy_websocket} |
%%                                                          {upgrade, protocol, cowboy_websocket, Req, Opts}
%% @end
%% -------------------------------------------------------------------
init(_Any, _Req, _Opt) ->
	{upgrade, protocol, cowboy_websocket}.

%% -------------------------------------------------------------------
%% @doc
%% Initialize the state for this session.
%%
%% @spec websocket_init(TransportName, Req, Opts) -> 	{ok, Req, State} |
%%                                                      {ok, Req, State, hibernate} |
%%                                                      {ok, Req, State, Timeout} |
%%                                                      {ok, Req, State, Timeout, hibernate} |
%%                                                      {shutdown, Req}
%% @end
%% -------------------------------------------------------------------
websocket_init(_TransportName, Req, _Opt) ->
	{Client, Req1} = cowboy_req:header(<<"x-forwarded-for">>, Req),
	State = #state{client = Client},
	{ok, Req1, State, hibernate}.

%% -------------------------------------------------------------------
%% @doc
%% Handle the data received from the Websocket connection.
%% 
%% @spec websocket_handle(InFrame, Req, State) ->   {ok, Req, State} |
%%                                                  {ok, Req, State, hibernate} |
%%                                                  {reply, OutFrame | [OutFrame], Req, State} |
%%                                                  {reply, OutFrame | [OutFrame], Req, State, hibernate} |
%%                                                  {shutdown, Req, State}
%% @end
%% -------------------------------------------------------------------
websocket_handle({text, Data}, Req, State) ->
	JsonTerm = jsx:decode(Data),
	Type = proplists:get_value(<<"type">>, JsonTerm),
	case Type of
		<<"GETROOM">> ->
			Room = generate_room(),
			DataRoom = iolist_to_binary(jsx:encode([{<<"type">>, <<"GETROOM">>}, {<<"value">>, Room}])),
			RoomState = State#state{room = Room},
			gproc:reg({p,l, Room}),
			{reply, {text, <<DataRoom/binary>>}, Req, RoomState, hibernate};
		<<"ENTERROOM">> ->
			Room = proplists:get_value(<<"value">>, JsonTerm),
			Participants = gproc:lookup_pids({p, l, Room}),
			case length(Participants) of
				1 ->
					gproc:reg({p, l, Room}),
					RoomState = State#state{room = Room},
					{ok, Req, RoomState, hibernate};
				_ ->
					DataRoom = iolist_to_binary(jsx:encode([{type, <<"WRONGROOM">>}])),
					{reply, {text, <<DataRoom/binary>>}, Req, State, hibernate}
			end;
		_ ->
			reply2peer(Data, State#state.room),
			{ok, Req, State, hibernate}
	end;

websocket_handle(_Any, Req, State) ->
	{ok, Req, State, hibernate}.

%% -------------------------------------------------------------------
%% @doc
%% Handle the Erlang message received.
%%
%% @spec websocket_info(Info, Req, State) ->    {ok, Req, State} |
%%                                              {ok, Req, State, hibernate} |
%%                                              {reply, OutFrame | [OutFrame], Req, State} |
%%                                              {reply, OutFrame | [OutFrame], Req, State, hibernate} |
%%                                              {shutdown, Req, State}
%% @end
%% -------------------------------------------------------------------
websocket_info(_Info, Req, State) ->
	{reply, {text, _Info}, Req, State, hibernate}.

%% -------------------------------------------------------------------
%% @doc
%% Perform any necessary cleanup of the state.
%%
%% @spec websocket_terminate(Reason, Req, State) -> ok
%% @end
%% -------------------------------------------------------------------
websocket_terminate(_Reason, _Req, _State) ->
	ok.

%% -------------------------------------------------------------------
%% @doc
%% Reply data to peer
%%
%% @spec reply2peer(Data, Room) -> [<<Data/binary>>]
%% @end
%% -------------------------------------------------------------------
reply2peer(Data, Room) ->
	[Pid ! <<Data/binary>> || Pid <- gproc:lookup_pids({p, l, Room}) -- [self()]].

%% -------------------------------------------------------------------
%% @doc
%% Generate random room number
%%
%% @spec generate_room() -> integer() >= 1
%% @end
%% -------------------------------------------------------------------
generate_room() ->
	rand:seed(exs1024, os:timestamp()),
	rand:uniform(999999).