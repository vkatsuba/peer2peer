-module(error_404).

-export([init/3, allowed_methods/2, content_types_provided/2]).

-export([as_html/2]).


init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  Methods = [
    <<"HEAD">>, <<"GET">>, <<"POST">>, <<"OPTIONS">>
  ],
  {Methods, Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"text/html">>, as_html}
  ], Req, State}.


as_html(Req, State) ->
  Body = <<"<!DOCTYPE html><html><head><title>404 - Not Found",
    "</title></head><body><h1>404 - Not Found</h1></body>",
    "</html>">>,
  {ok, Req2} = cowboy_req:reply(404, [], Body, Req),
  {halt, Req2, State}.