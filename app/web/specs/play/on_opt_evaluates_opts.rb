# opts are still evaluated when on: is false (a side effect shows it)
x = 0
play 60, on: false, amp: (x += 1)
puts x
