-module(book_route).

-behaviour(cowboy_rest).

-export([init/2]).

-export([allowed_methods/2]).

-export([content_types_provided/2]).

-export([content_types_accepted/2]).

-export([known_methods/2]).

%% Callback Callbacks
-export([get_book/2]).

-export([post_book/2]).% init(Req0, Opts) ->

init(Req0, State) -> {cowboy_rest, Req0, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_book}],
     Req,
     State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, post_book}],
     Req,
     State}.

get_book(Req0, State0) ->
    QsVals = cowboy_req:parse_qs(Req0),
    case lists:keyfind(<<"isbn">>, 1, QsVals) of
        {_, undefined} -> Message = {[{response, <<"Hello">>}]};
        {_, ISBN} ->
            Message = io:format("GET /book isbn=~p ~n", [ISBN]),
            {[{response, library:get(ISBN)}]}
    end,
    {jiffy:encode(Message), Req0, State0}.

post_book(Req0, _State0) ->
    {ok, EncodedData, _} = cowboy_req:read_body(Req0),
    DecodedData = jiffy:decode(EncodedData),

    case DecodedData of
        {[{<<"isbn">>, undefined}, {<<"book">>, undefined}]} ->
            {Reply, Code} = {{response, <<"undefined attributed">>},
                             204};
        {[{<<"isbn">>, undefined}, {<<"book">>, _}]} ->
            {Reply, Code} = {{response, <<"undefined isbn">>}, 206};
        {[{<<"isbn">>, _}, {<<"book">>, undefined}]} ->
            {Reply, Code} = {{response, <<"undefined book">>}, 206};
        {[{<<"isbn">>, ISBN}, {<<"book">>, Book}]} ->
            {R, Code} = library:post(ISBN, Book),
            io:format("POST /book isbn=~p book=~p~n", [ISBN, Book]),
            Reply = {response, R}
    end,
    EncodedReply = jiffy:encode({[Reply]}),

    cowboy_req:reply(Code,
                     #{<<"content-type">> => <<"application/json">>},
                     EncodedReply,
                     Req0).

known_methods(Req, State) ->
    Result = [<<"GET">>, <<"POST">>],
    {Result, Req, State}.
