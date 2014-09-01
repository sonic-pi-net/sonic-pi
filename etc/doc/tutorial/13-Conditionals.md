# Conditionals

A common thing you'll likely find yourself wanting to do is to not only play a random note (see the previous section on randomness) but also make a random decision and based on the outcome run some code or some other code. For example, you might want to randomly play a drum or a cymbal. We can achieve this with an `if` statement.

## Flipping a Coin 

So, let's flip a coin, if it's heads, play a drum, if it's tails play a cymbal. Easy. We can emulate a coin flip with our `one_in` function (introduced in the section on randomness) specifying a probability of 1 in 2: `one_in(2)`. We can then use the result of this to decide between two pieces of code, the code to play the drum and the code to play the cymbal:

```
loop do

  if one_in(2)
    sample :drum_heavy_kick
  else
    sample :drum_cymbal_closed
  end
  
  sleep 0.5
  
end
```

Notice that `if` statements have three parts:

* The question to ask
* The first choice of code to run (if answer to the question is yes)
* The second choice of code to run (if the answer to the question is no)

Typically in programming languages, the notion of yes is represented by the term `true` and the notion of no is represented by the term `false`. So we need to find a question that will give us `true` or `false` values which is exactly what `one_in` does. 

Notice how the first choice is wrapped between the `if` and the `else` and the second choice is wrapped between the `else` and the `end`. Just like do/end blocks you can put multiple lines of code in either place. For example:

```
loop do

  if one_in(2)
    sample :drum_heavy_kick
    sleep 0.5
  else
    sample :drum_cymbal_closed
    sleep 0.25
  end
  
end
```

This time we're now sleeping for a different amount of time depending on which choice we make.


## Simple if

Sometimes you want optionally execute just one line of code. This is possible by placing `if` and then the question at the end. For example:

```
use_synth :dsaw

loop do
  play 50, amp: 0.3, release: 2
  play 53, amp: 0.3, release: 2 if one_in(2)
  play 57, amp: 0.3, release: 2 if one_in(3)
  play 60, amp: 0.3, release: 2 if one_in(4)
  sleep 1.5
end
```

This will chords of different numbers with the chance of each note playing having a different probability.
