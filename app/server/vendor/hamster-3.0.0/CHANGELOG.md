Changelog
=========

  - 3.0.0
    * [Changed] the behavior of Hamster::Hash#values_at
    * [Added] Hamster::Hash#dig and Hamster::Enumerable#grep_v
    * [Added] Hamster::Enumerable#grep_v
    * [Added] Hamster::Hash#>
    * [Added] Hamster::Hash#>=
    * [Added] Hamster::Hash#<

  - 2.0.0
    * [Removing] Hamster.deque since talking to the classes should be the main entry point
    * [Removing] Hamster.mutable_set
    * [Removing] Hamster.mutable_queue
    * [Removing] Hamster.hash
    * [Adding] an module for association behavior
    * [Changing] Enumerable#to_list to a more susinct behavior
    * [Adding] the Struct#to_h method if Struct doesn't have that method [< 1.9.3]
    * [Changing] Freezing the result of allocating a Hamster::Deque
    * [Documenting] the Hamster::Empty constants as private interfaces
  - 1.0.0
    * Specified benchmark-ips version
  - 0.X.Y
    * ...?
