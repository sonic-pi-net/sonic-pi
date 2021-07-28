

* Perhaps remove the link_ from the erlang function names? Might be sort of redundant since it is alrady in package sp_link...?

* Useful links:
- https://github.com/Ableton/link/blob/master/include/ableton/Link.hpp
- http://ableton.github.io/link/

Session State is a class that contains:
-


The API is:
- `enable`
- `is_enabled`

- `get_tempo`
- `set_tempo`

- `get_num_peers`

- `get_beat_at_time`
- `get_phase_at_time`
- `get_time_at_beat`
- `request_beat_at_time`
- `force_beat_at_time`

- `set_is_playing`
- `get_is_playing`
- `get_time_for_is_playing`

- `is_start_stop_sync_enabled`
- `start_stop_sync_enable`

- `request_beat_at_start_playing_time`
- `set_is_playing_and_request_beat_at_time`

- `get_current_time_microseconds` : an aux function to get current time in microseconds. See _clock()_ method in Link. See __sp_get_current_time_microseconds__
- `set_callback_pid`: a function to register a callback for the Link callbacks. See __sp_set_callback_pid_nif__

The callbacks send the following tuples to the callback pid:
- `{link_num_peers, NumPeers}`
- `{link_tempo, Tempo}`
- `{link_stop}`
- `{link_start}`

Need C99 compliant compiler, since I am using C bools, that should not be an issue in this day and age, but keep an eye

* When passing parameters from erlang, careful that integers are not converted to double automatically, so need to be explicit

* To be notified and be able to affect start / stop status you need to enable Start / Stop sync, by calling __link_start_stop_sync_enable(true)__
