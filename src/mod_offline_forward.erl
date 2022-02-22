-module(mod_offline_forward).

-behaviour(gen_mod).
 
-export([start/2,
        stop/1, 
        mod_options/1,
        mod_opt_type/1, 
        depends/2, 
        create_message/1, 
        mod_doc/0]).

-include("scram.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
%% Required by ?T macro
-include("translate.hrl"). 

start(_Host, _Opt) ->
    ?INFO_MSG("starting mod_offline_forward", []),
    inets:start(),
    ?INFO_MSG("HTTP client started", []),
    ejabberd_hooks:add(offline_message_hook, _Host, ?MODULE, create_message, 50).  
 
stop (_Host) ->
    ?INFO_MSG("stopping mod_offline_forward", []),
    ejabberd_hooks:delete(offline_message_hook, _Host, ?MODULE, create_message, 50).

depends(_Host, _Opts) ->
    [].

mod_doc() ->
    #{desc =>
            ?T("Forward offline messages to HTTP endpoint.")}.

mod_options(_Host) ->
  [{auth_token, <<"secret">>},
  {remote_url, <<"http://example.com/test">>}].

mod_opt_type(auth_token) ->
  fun iolist_to_binary/1;

mod_opt_type(remote_url) ->
  fun iolist_to_binary/1.

create_message({Action, Packet} = Acc) when (Packet#message.type == chat) ->
    Vhost = (Packet#message.to)#jid.lserver,
    BadgeCount = mod_offline:get_queue_length((Packet#message.to)#jid.luser, Vhost),
    forward_message(Packet, BadgeCount),
    Acc;

create_message({Action, Packet} = Acc) ->
    case misc:unwrap_mucsub_message(Packet) of
        #message{} = Msg ->
            Vhost = (Msg#message.to)#jid.lserver,
            BadgeCount = mod_offline:get_queue_length((Msg#message.to)#jid.luser, Vhost),
            forward_message(Packet, BadgeCount);
	    _ ->
            Packet
    end,
    Acc;

create_message(Acc) ->
    Acc.

forward_message(Packet, BadgeCount) ->
    Host = (Packet#message.to)#jid.lserver,
    
    RemoteUrl = gen_mod:get_module_opt(Host, ?MODULE, remote_url),
    Token = gen_mod:get_module_opt(Host, ?MODULE, auth_token),
  
    Stanza = fxml:element_to_binary(xmpp:encode(Packet)),

     Data = string:join(["{",
        "\"stanza\": \"", binary_to_list(Stanza), "\", ",
        "\"badge\": \"", integer_to_list(BadgeCount), "\"",
    "}"], ""),

    Request = {binary_to_list(RemoteUrl), [{"Authorization", binary_to_list(Token)}], "application/json", Data},
    httpc:request(post, Request,[],[]).
