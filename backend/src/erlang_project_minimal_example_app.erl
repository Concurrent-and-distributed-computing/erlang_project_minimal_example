%%%-------------------------------------------------------------------
%% @doc erlang_project_skeleton public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_project_minimal_example_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    library:start(),
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/health", health_route, []},
                                        {"/book", book_route, []},
                                        {"/books", books_route, []}]}]),
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch},
                                   middlewares =>
                                       [cowboy_router,
                                        ca_cowboy_middleware,
                                        cowboy_handler]}),
    % populating database with data
    library:post(<<"9780130888938">>,
                 <<"Distributed Systems Principles and Paradigms">>),
    library:post(<<"0132143011">>,
                 <<"Distributed Systems: Concepts and Design">>),
    library:post(<<"0849331781">>,
                 <<"Distributed System Design">>),
    library:post(<<"3540606270">>,
                 <<"Disconnected Operation in a Distributed "
                   "File System">>),
    library:post(<<"9781430210597">>,
                 <<"Automating Linux and Unix System Administration">>),
    erlang_project_minimal_example_sup:start_link().

stop(_State) -> ok = cowboy:stop_listener(http).

%% internal functions

