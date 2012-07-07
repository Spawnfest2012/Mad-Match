-record(pinger,{ id :: pos_integer(),
                 name :: string(),
                 type :: atom() ,
                 user_id :: pos_integer(),
                 end_point :: string(),
                 frecuency :: pos_integer(),
                 created_at
                }).
-record(subscription,{ id             :: pos_integer(),
                       type           :: email | tweet,
                       user_id        :: pos_integer(),
                       pinger_id      :: pos_integer(),
                       down_time      :: pos_integer(),
                       notify_when_up :: boolean(),
                        created_at}).
-record(event,{ type      :: pinger_down | pinger_up,
                pinger_id :: pos_integer(),
                down_time :: pos_integer()}).


-record(user, {id :: pos_integer(),name :: binary(),email :: binary(),password :: binary(),tagline :: binary(),created_at}).
