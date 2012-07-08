-record(pinger,{ id :: pos_integer(),
                 name :: string(),
                 type :: ping|dns|http,
                 user_id :: pos_integer(),
                 end_point :: string(),
                 frequency :: pos_integer(),
                 last_status :: up | down, 
                 last_check :: pos_integer(),
                 data = [] :: list(), % local data to extend pinger basic info
                 subscription_count :: pos_integer(),
                 user_name :: string(),
                 user_tagline :: string(),
                 created_at :: term()
                }).
-record(subscription,{ id             :: pos_integer(),
                       type           :: email | twitter,
                       user_id        :: pos_integer(),
                       pinger_id      :: pos_integer(),
                       down_time      :: pos_integer(),
                       notify_when_up :: boolean(),
                       created_at :: term()}).
-record(event,{ type      :: pinger_down | pinger_up,
                pinger    :: #pinger{},
                down_time :: pos_integer()}).


-record(user, {id :: pos_integer(),
               name :: binary(),
               email :: binary(),
               password :: binary(),
               twitter :: binary(),
               tagline :: binary(),
               created_at :: term()}).
