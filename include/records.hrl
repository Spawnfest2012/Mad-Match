-record(pinger,{ id :: pos_integer(),
                 name :: string(),
                 type :: ping|dns|http,
                 user_id :: pos_integer(),
                 end_point :: string(),
                 frequency :: pos_integer(), %% milliseconds
                 last_status :: undefined | up | down,
                 last_check = 0 :: pos_integer(), %% milliseconds
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
                       down_time      :: pos_integer(), %% milliseconds
                       notification_delay :: pos_integer(), %% milliseconds
                       last_notification = 0 :: pos_integer(), %% milliseconds
                       notify_when_up :: boolean(),
                       created_at :: term()}).
-record(event,{ type      :: pinger_down | pinger_up,
                pinger    :: #pinger{},
                 down_time :: pos_integer() %% milliseconds
              }).


-record(user, {id :: pos_integer(),
               name :: binary(),
               email :: binary(),
               password :: binary(),
               twitter :: binary(),
               tagline :: binary(),
               created_at :: term()}).
