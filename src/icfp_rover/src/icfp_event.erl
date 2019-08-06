-module(icfp_event).
-export([start_link/0]).
-export([notify/1, add_handler/2]).

start_link() ->
	gen_event:start_link({local, ?MODULE}).

notify(Event) ->
	gen_event:notify(?MODULE, Event).

add_handler(Handler, State) ->
	gen_event:add_handler(?MODULE, Handler, State).
