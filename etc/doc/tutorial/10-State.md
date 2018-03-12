10 Time State

# Time State

Often it is useful to have information that is *shared across multiple
threads or live loops*. For example, you might want to share a notion of
the current key, BPM or even more abstract concepts such as the current
'complexity' (which you'd potentially interpret in different ways across
different threads). We also don't want to lose any of our existing
determinism guarantees when doing this. In other words, we'd still like
to be able to share code with others and know exactly what they'll hear
when they run it. At the end of Section 5.6 of this tutorial we briefly
discussed why we *should not use variables to share information across
threads* due to a loss of determinism (in turn due to race conditions).

Sonic Pi's solution to the problem of easily working with global
variables in a deterministic way is through a novel system it calls Time State. This
might sound complex and difficult (in fact, in the UK, programming with
multiple threads and shared memory is typically a university level
subject). However, as you'll see, just like playing your first note,
*Sonic Pi makes it incredibly simple to share state across threads*
whilst still keeping your programs *thread-safe and deterministic.*.

Meet `get` and `set`...
