-module(books_route).

-behaviour(cowboy_rest).

-export([init/2]).

-export([allowed_methods/2]).

-export([content_types_provided/2]).

-export([known_methods/2]).

-export([get_books/2]).

init(Req0, State) ->
    % {ok, ReqWithCorsHeaders} = set_cors_headers(Req0),
    {cowboy_rest, Req0, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_books}],
     Req,
     State}.

get_books(Req, State) ->
    Response = library:gets(),
    {jiffy:encode(Response), Req, State}.

known_methods(Req, State) ->
    Result = [<<"GET">>],
    {Result, Req, State}.
