-module(library).

-behaviour(gen_server).

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

-export([start/0]).

-export([get/1, gets/0, post/2]).

% These are all wrappers for calls to the server
get(ISBN) -> gen_server:call(?MODULE, {get, ISBN}).

gets() -> gen_server:call(?MODULE, {get}).

post(ISBN, Book) ->
    gen_server:call(?MODULE, {post, ISBN, Book}).

start() ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).

% This is called when a connection is made to the server
init([]) ->
    Library = dict:new(),
    {ok, Library}.

% handle_call is invoked in response to gen_server:call
handle_call({post, ISBN, Book}, _From, Library) ->
    Response = case dict:is_key(ISBN, Library) of
                   true ->
                       NewLibrary = Library,
                       {<<"book already exists">>, 409}; % http 409 conflict
                   false ->
                       NewLibrary = dict:append(ISBN, Book, Library),
                       {<<"book added">>, 201} % http 201 created
               end,
    {reply, Response, NewLibrary};
handle_call({get, ISBN}, _From, Library) ->
    Response = case dict:is_key(ISBN, Library) of
                   true ->
                       book,
                       dict:fetch(ISBN, Library);
                   false -> <<"not found">>
               end,
    {reply, Response, Library};
handle_call({get}, _From, Library) ->
    Keys = dict:fetch_keys(Library),
    Response = [#{<<"isbn">> => K,
                  <<"book">> => dict:fetch(K, Library)}
                || K <- Keys],
    {reply, Response, Library}.

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, Library) -> {noreply, Library}.

handle_info(_Message, Library) -> {noreply, Library}.

terminate(_Reason, _Library) -> ok.

code_change(_OldVersion, Library, _Extra) ->
    {ok, Library}.
