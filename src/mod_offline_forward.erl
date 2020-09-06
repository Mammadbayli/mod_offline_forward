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
  Type = fxml:get_path_s(xmpp:encode(Packet), [{elem,list_to_binary("data")}, {attr, list_to_binary("type")}]),
  case Type of
    <<"text">> ->  
		  [{text, _, Body}] = Packet#message.body;
    <<"audio">> -> 
			Body = list_to_binary("Voice");
		<<"photo">> -> 
			Body = list_to_binary("Photo")
  end,

  post_offline_message(Packet#message.from, Packet#message.to, Type, Body, Packet#message.id),
  Acc;

create_message(Acc) ->
  Acc.

post_offline_message(From, To, Type, Body, MessageId) ->
  ?INFO_MSG("Posting From ~p To ~p Body ~p ID ~p~n",[From, To, Body, $
  ToUser = To#jid.luser,
  FromUser = From#jid.luser,
%   Vhost = To#jid.lserver,
  Data = string:join(["{",
    "\"to\": \"", binary_to_list(ToUser), "\", ",
    "\"from\": \"", binary_to_list(FromUser), "\", ",
    "\"type\": \"", binary_to_list(Type), "\", ",
    "\"body\": \"", binary_to_list(Body), "\", ",
    "\"messageId\": \"", binary_to_list(MessageId), "\"",
  "}"], ""),
  Request = {"http://api.mammadbayli.com/notify", [{"Authorization", $
  httpc:request(post, Request,[],[]),
  ?INFO_MSG("post request sent", []).

