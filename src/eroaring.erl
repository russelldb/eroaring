-module(eroaring).

-export([
         new/0,
         add/2,
         add_all/2,
         cardinality/1,
         contains/2,
         remove/2,
         run_optimize/1,
         serialize/1,
         deserialize/1,
         union/2
        ]).
-export_type([
    bits/0
]).

-on_load(init/0).

% The name of the application we're writing. This is the name
% used for the Erlang .app file.

-define(APPNAME, eroaring).

% The name of the shared library we're going to load the NIF
% code from. Defined in rebar.config as so_name.

-define(LIBNAME, eroaring).

% should be opaque, but erlc complains if that's exported
-type bits() :: term().

%% API

% NIF functions end up overriding the functions defined in this module. But
% this module must define the functions we want the NIF to implement.
% Theoretically this won't ever get called as out on_load function init/0
% should raise an error if we have issues.
%
% A really nice person would make a pure Erlang fallback incase a NIF was
% unable to load for a specific platform.

-spec new() -> bits() | {error, term()}.
new() ->
    not_loaded(?LINE).

serialize(_) ->
    not_loaded(?LINE).

deserialize(_) ->
    not_loaded(?LINE).

union(_, _) ->
    not_loaded(?LINE).

add(_, _) ->
    not_loaded(?LINE).

add_all(_, _) ->
    not_loaded(?LINE).

remove(_, _) ->
    not_loaded(?LINE).

contains(_, _) ->
    not_loaded(?LINE).

cardinality(_) ->
    not_loaded(?LINE).

-spec run_optimize(bits()) -> bits() | {error, term()}.
run_optimize(_) ->
    not_loaded(?LINE).

%% Iternal functions

% Since we used init/0 in our -on_load() preprocessor directive, this
% function will get called as the module is loaded. This is the perfect
% place to load up our NIF shared library. Handily, the response of
% erlang:load_nif/2 matches the return specification for -on_load()
% functions.

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

% This is just a simple place holder. It mostly shouldn't ever be called
% unless there was an unexpected error loading the NIF shared library.

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
