-module(main).
-export([main/1]).

main([Host, Port]) ->
    application:start(icfp_rover),
    icfp_client:connect(Host, list_to_integer(Port)).