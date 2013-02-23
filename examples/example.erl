-module(example).
-export([ex1/0]).

-include("epushmsg.hrl").

%% example of sending a push notification to a specific user
%% returns status code (200 if successful)
-spec ex1() -> integer().
ex1() ->
    hackney:start(),
    PushMsgParams = epushmsg:new_params(),
    Payload = <<"{
                        \"aps\": {
                            \"badge\": \"+1\",
                            \"alert\": \"Hello World!\",
                            \"sound\": \"default\"
                        },
                        \"aliases\": [\"diginux@gmail.com\"],
                        \"android\": {
                            \"alert\": \"Hello World!\",
                            \"extra\": {\"type\":\"order\"}
                        },
                        \"type\": \"order\"
                    }">>,
    PushMsgParams2 = PushMsgParams#pushmsg_params{payload=Payload},
    epushmsg:push(PushMsgParams2).
