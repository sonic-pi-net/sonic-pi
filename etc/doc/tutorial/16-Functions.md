# Functions

Once you start writing lots of code, you may wish to find a way to organise and structure things to make them tidier and easier to understand. Functions are a very powerful way to do this. They give us the ability to give a name to a bunch of code. Let's take a look.

## Defining functions

```
define :foo do
  play 50
  sleep 1
  play 55
  sleep 2
end
```

Here, we've define a new function called `foo`. We do this with our old friend the do/end block and the magic code `define` followed by the name we which to give our function. We didn't have to call it `foo` we could have called it anything we want such as `bar`, `baz` or ideally something meaningful to you like `main_section` or `lead_riff`. 

Remember to add a colon `:` to the name of your function when you define it.

## Calling functions

Once we have defined our function we can call it by just writing its name:

```
define :foo do
  play 50
  sleep 1
  play 55
  sleep 0.5
end

foo

sleep 1

2.times do
  foo
end
```

We can even use `foo` inside iteration blocks or anywhere we may have written `play` or `sample`. This gives us a great way to express ourselves and to create new meaningful words to use in our compositions.

## Functions are remembered across runs

So far, every time you've pressed the run button, Sonic Pi has started from a completely blank slate. It knows nothing except for what is in the workspace. You can't reference code in another workspace or another thread. However, functions change that. When you define a function, Sonic Pi *remembers* it. Let's try it. Delete all the code in your workspace and replace it with:

```
foo
```

Press the run button - and hear your function play. Where did the code go? How did Sonic Pi know what to play? Sonic Pi just remembered your function - so even after you deleted it from the workspace, it remembered what you had typed. This behaviour only works with functions starting with the letters `def...` such as `defonce`.

## Parameterised functions

For those of you that would like to see a more advanced feature of functions you might be interested in knowing that just like you can pass min and max values to `rrand` you can teach your functions to accept arguments. Let's take a look:

```
define :my_player do |n|
  play n
end

my_player 80
sleep 0.5
my_player 90
```

This isn't very exciting but it illustrates the point. We've created our own version of `play` called `my_player` which is parameterised.

The parameters need to go after the `do` of the `define` do/end block,  surrounded by vertical goalposts `|`. and separated by commas `,`. You may use any words you want for the parameter names. 

The magic happens inside the `define` do/end block. You may use the parameter names as if they were real values. In this example I'm playing note `n`.  You can consider the parameters as a kind of promise that when the code runs, they will be replaced with actual values. You do this by passing a parameter to the function when you call it. I do this with `my_player 80` to play note 80. Inside the function definition, `n` is now replaced with 80, so `play n` turns into `play 80`. When I call it again with `my_player 90`, `n` is now replaced with 90, so `play n` turns into `play 90`.

Let's see a more interesting example:


``` 
define :chord_player do |root, repeats| 
  repeats.times do
    play chord(root, :minor), release: 0.3
    sleep 0.5
  end
end

chord_player :e3, 2
sleep 0.5
chord_player :a3, 3
chord_player :g3, 4
sleep 0.5
chord_player :e3, 3

```

Here I used `repeats` as if it was a number in the line `repeats.times do`. I also used `root` as if it was a note name in my call to `play`. 


See how we're able to write something very expressive and easy to read by moving a lot of our logic into a function!
