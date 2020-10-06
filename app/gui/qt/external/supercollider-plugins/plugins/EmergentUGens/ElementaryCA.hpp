/*
  This file is part of supercollider-plugins
  Full source at https://github.com/ethancrawford/supercollider-plugins
  License: https://github.com/ethancrawford/supercollider-plugins/LICENSE

  This file was adapted from parallel_elementary_cellular_automata
  (https://github.com/knotman90/parallel_elementary_cellular_automata).
*/

#include <bitset>
#include <cassert>
#include <vector>
#include <sstream>
#include "RTAllocator.hpp"

#ifndef _CA_H
#define _CA_H

//////////////////////////////
//	    class Rule          //
//////////////////////////////
class Rule {
public:
  Rule() {};
  Rule(const int N) : rule_num(N) { set_rule(N); }

  bool get_next_state(const bool left, const bool center,
    const bool right) const {
    const unsigned int idx = 4 * !left + 2 * !center + !right;
    return rule[7 - idx];
  }
  unsigned int get_rule_number() const { return rule_num; }

private:
  unsigned int rule_num; // change to uint_8;
  std::bitset<8> rule;

  void set_rule(const unsigned int N) {
    rule_num = N;
    for (int i = 0; i < 8; ++i) {
      rule[i] = (N >> i) & 1;
    }
  }
};

//////////////////////////////
//	    class Iteration     //
//////////////////////////////
template <class Child> class Iteration {
public:
  Iteration() {};
  inline void set(const size_t idx, bool value) {
    assert(idx < size());
    static_cast<Child*>(this)->set(idx, value);
  };
  inline bool get(const size_t idx) {
    assert(idx < size());
    return static_cast<Child*>(this)->get(idx);
  }

  inline bool size() { return static_cast<Child*>(this)->size(); }
};

template <unsigned int S> class Iteration_Bitset : public Iteration<Iteration_Bitset<S>> {
public:
  Iteration_Bitset() : space(0) {};

  inline void set(const size_t idx, bool value) { space[idx] = value; }

  inline bool get(const size_t idx) const { return space[idx]; }

  constexpr inline unsigned int size() const { return S; }

  std::bitset<S> space;
};

class Iteration_Vector {
public:
  Iteration_Vector(const int _size) : size(_size), space(_size) {};

  inline void set(const size_t idx, bool value) { space[idx] = value; }

  inline bool get(const size_t idx) const { return space[idx]; }

  constexpr inline unsigned int get_size() const { return size; }

private:
  const unsigned int size;
  std::vector<bool, rt_allocator<bool>> space;
};

template <typename SpaceType> class CA {
public:
  using space_imp = SpaceType;
  using Cell_space = std::vector<SpaceType, rt_allocator<SpaceType>>;

  CA() {}

  void init(const unsigned int _rule_n, const unsigned int _size, const unsigned int _max_iterations,
    const space_imp& initial_state) {
    rule = Rule(_rule_n);
    size = _size;
    max_iterations = _max_iterations;
    assert(max_iterations > 0);
    assert(size > 0);

    space.reserve(max_iterations);
    space.push_back(initial_state);
  }

  void evolve() {
    for (unsigned int i = 0; i < max_iterations; ++i) {
      space.push_back(evolve_iteration(space[i]));
    }
  }

  space_imp evolve_iteration(space_imp& iteration) {
    space_imp next_iteration(size);
    for (unsigned int i = 1; i < size - 1; i++) {
      const bool left = iteration.get(i - 1);
      const bool center = iteration.get(i);
      const bool right = iteration.get(i + 1);
      next_iteration.set(i, rule.get_next_state(left, center, right));
    }
    next_iteration.set(0, rule.get_next_state(0, iteration.get(0), iteration.get(1)));
    next_iteration.set(
      size - 1,
      rule.get_next_state(iteration.get(size - 2), iteration.get(size - 1), 0)
    );

    return next_iteration;
  }

  void store(int* m_partials_flags, int active_waves) {
    int iterations = (int)(active_waves / size);
    int i = 0, j = 1;
    bool keep_going = true;
    while (i < iterations && keep_going) {
      while (j <= size && keep_going) {
        if (((i * size) + j) >= (active_waves - 1)) {
          keep_going = false;
        }
        m_partials_flags[1 + (i * size) + j] = (float)space[i].get(j - 1);
        j++;
      }
      i++;
      j = 1;
    }
  }

  void operator()() { evolve(); }

  unsigned int get_rule_number() { return rule.get_rule_number(); }

  unsigned int get_size() { return size; }

  unsigned int get_max_iterations() { return max_iterations; }

private:
  Rule rule;
  unsigned int size;
  unsigned int max_iterations;
  Cell_space space;
};


#endif //_CA_H
