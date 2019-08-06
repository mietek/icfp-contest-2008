-module(icfp_client_proc).
-behavior(gen_server).
-export([start_link/2]).
-export([connect/0, connect/2, send/1]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-record(state, {
	host,
	port,
	socket,
	acc = []
}).

start_link(Host, Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).

connect() ->
	gen_server:call(?MODULE, connect).

connect(Host, Port) ->
	gen_server:call(?MODULE, {connect, Host, Port}).

send(Cmd) ->
	gen_server:call(?MODULE, {send, Cmd}).

init([Host, Port]) ->
	{ok, #state{host = Host, port = Port}}.

handle_call(connect, _From, State) ->
	{ok, Socket} = gen_tcp:connect(State#state.host, State#state.port, [{active, true}]),
	{reply, ok, State#state{socket = Socket}};
handle_call({connect, Host, Port}, _From, State) ->
	{ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}]),
	{reply, ok, State#state{host = Host, port = Port, socket = Socket}};
handle_call({send, Cmd},  _From, State) ->
	send_command(State#state.socket, Cmd),
	{reply, ok, State};
handle_call(_Req, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, _Socket, Packet}, State) ->
	Acc = dispatch(State#state.socket, State#state.acc ++ Packet),
	{noreply, State#state{acc = Acc}};
handle_info({tcp_error, _Socket, Reason}, State) ->
	error_logger:error_report({icfp_client, {tcp_error, Reason}}),
	{noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
	{stop, tcp_closed, State};
handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_, #state{socket = undefined}) ->
	ok;
terminate(_, State) ->
	gen_tcp:close(State#state.socket).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

dispatch(_, []) ->
	[];
dispatch(Socket, Acc) ->
	case icfp_client_proto:parse(Acc) of
		{[], Rest} ->
			Rest;
		{P, Rest} ->
			icfp_event:notify(P),
			dispatch(Socket, Rest)
	end.

send_command(_Socket, noop) ->
	ok;
send_command(Socket, Cmd) ->
	gen_tcp:send(Socket, icfp_client_proto:encode(Cmd)).

