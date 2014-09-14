# This is a TOML document. Boom.

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
organization = "GitHub"
bio = "GitHub Cofounder & CEO\nLikes tater tots and beer."
dob = 1979-05-27T07:32:00Z # First class dates? Why not?

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # You can indent as you please. Tabs or spaces. TOML don't care.
  [servers.alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.beta]
  ip = "10.0.0.2"
  dc = "eqdc10"

[clients]
data = [ ["gamma", "delta"], [1, 2] ] # just an update to make sure parsers support it

# Line breaks are OK when inside arrays
hosts = [
  "alpha",
  "omega"
]

funky = "I'm a string. \"You can quote me\". Tab \t newline \n you get it."
right = "C:\\Users\\nodejs\\templates"
wrong = "C:\Users\nodejs\templates" # note: doesn't produce a valid path

# Test file for TOML
# Only this one tries to emulate a TOML file written by a user of the kind of parser writers probably hate
# This part you'll really hate

[the]
test_string = "You'll hate me after this - #"          # " Annoying, isn't it?

    [the.hard]
    test_array = [ "] ", " # "]      # ] There you go, parse this!
    test_array2 = [ "Test #11 ]proved that", "Experiment #9 was a success" ]
    # You didn't think it'd as easy as chucking out the last #, did you?
    another_test_string = " Same thing, but with a string #"
    harder_test_string = " And when \"'s are in the string, along with # \""   # "and comments are there too"
    # Things will get harder
    
        [the.hard.bit#]
        what? = "You don't think some user won't do that?"
        multi_line_array = [
            "]",
            "Oi!\n",
            # ] Oh yes I did
            ]

東京都 = 123
