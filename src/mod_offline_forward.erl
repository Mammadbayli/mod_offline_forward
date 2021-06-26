-module(mod_offline_forward).

-behaviour(gen_mod).
 
-export([start/2, stop/1, mod_options/1, depends/2, create_message/1]).

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
    Type = fxml:get_path_s(xmpp:encode(Packet), [{elem, <<"data">>)}, {attr, <<"type">>}]),
    
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

    To = (Packet#message.to)#jid.luser,
    From = (Packet#message.from)#jid.luser,
    
    Vhost = (Packet#message.to)#jid.lserver,
    BadgeCount = mod_offline:get_queue_length((Packet#message.to)#jid.luser, Vhost),

    post_offline_message(From, To, Body, BadgeCount),
    
    Acc;

create_message({Action, Packet} = Acc) ->
    case misc:unwrap_mucsub_message(Packet) of
        #message{} = Msg ->
	    [{text, _, Subject}] = Msg#message.subject,
            To = (Msg#message.to)#jid.luser,
		    
            User = (Msg#message.from)#jid.luser,
	    Resource = (Msg#message.from)#jid.resource,
	    From = [Resource/binary, <<"@">>/binary, User/binary],

	    Vhost = (Msg#message.to)#jid.lserver,
            BadgeCount = mod_offline:get_queue_length((Msg#message.to)#jid.luser, Vhost),

            post_offline_message(From, To, Subject, BadgeCount);
	_ ->
            Packet
    end,

    Acc;

create_message(Acc) ->
    Acc.

post_offline_message(From, To, Body, BadgeCount) ->	
    BinaryCount = integer_to_binary(BadgeCount),
    Data = << <<"{\"to\": \"">>/binary, To/binary, <<"\", \"from\": \"">>/binary, From/binary, <<"\", \"body\": \"">>/binary, Body/binary, <<"\", \"badge\": ">>/binary, BinaryCount/binary, <<"}">>/binary >>,
  
    ?INFO_MSG("Posting Data ~p~n",[Data]),
    Request = { string:join([os:getenv("NFS_API_URL"),"/notify"], ""), [{"Authorization", os:getenv("NFS_API_KEY")}], "application/json", Data },
    httpc:request(post, Request,[],[]),
    ?INFO_MSG("post request sent", []).
