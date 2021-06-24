-module(mod_offline_forward).

-behaviour(gen_mod).
 
-export([start/2, stop/1, mod_options/1, depends/2, create_message/1]).

%-include("misc.erl").
-include("scram.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
 
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

mod_options(_Host) ->
    [].
 
create_message({Action, Packet} = Acc) when (Packet#message.type == chat) ->
    Type = fxml:get_path_s(xmpp:encode(Packet), [{elem,list_to_binary("data")}, {attr, list_to_binary("type")}]),
    
    case Type of
        <<"text">> ->  
            [{text, _, Body}] = Packet#message.body;
        <<"audio">> -> 
	    Body = "Voice";
	<<"photo">> -> 
	    Body = "Photo";
	_ -> 
            Body = "Mesage"
    end,

    To = binary_to_list((Packet#message.to)#jid.luser),
    From = binary_to_list((Packet#message.from)#jid.luser),
    
    Vhost = (Packet#message.to)#jid.lserver,
    BadgeCount = mod_offline:get_queue_length((Packet#message.to)#jid.luser, Vhost),

    post_offline_message(From, To, binary_to_list(Body), BadgeCount),
    
    Acc;

create_message({Action, Packet} = Acc) ->
    case misc:unwrap_mucsub_message(Packet) of
        #message{} = Msg ->
	    [{text, _, Body}] = Msg#message.body,
	    User = binary_to_list((Msg#message.from)#jid.user),
	    Resource = binary_to_list((Msg#message.from)#jid.resource),
            To = binary_to_list((Msg#message.to)#jid.luser),
	    From = string:join([Resource, User],"@"),

	    Vhost = (Msg#message.to)#jid.lserver,
            BadgeCount = mod_offline:get_queue_length((Msg#message.to)#jid.luser, Vhost),

            post_offline_message(From, To, binary_to_list(Body), BadgeCount);
	_ ->
            Packet
    end,

    Acc;

create_message(Acc) ->
    Acc.

post_offline_message(From, To, Body, BadgeCount) ->
    ?INFO_MSG("Posting From ~p To ~p Body ~p ID ~p~n",[From, To, Body, BadgeCount]),
 
    Data = string:join(["{",
        "\"to\": \"", To, "\", ",
        "\"from\": \"", From, "\", ",
        "\"body\": \"", Body, "\", ",
        "\"badge\": \"", integer_to_list(BadgeCount), "\"",
    "}"], ""),
  
    Request = {string:join([os:getenv("NFS_API_URL"),"/notify"], ""), [{"Authorization", os:getenv("NFS_API_KEY")}], "application/json", Data},
    httpc:request(post, Request,[],[]),
    ?INFO_MSG("post request sent", []).
