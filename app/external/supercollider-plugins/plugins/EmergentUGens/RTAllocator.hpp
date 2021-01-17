#include "SC_World.h"
#include "SC_InterfaceTable.h"

#ifndef _RTALLOCATOR_H
#define _RTALLOCATOR_H

extern World* g_pWorld;
extern InterfaceTable* ft;

template <typename T>
class rt_allocator {
public:
  typedef size_t size_type;
  typedef ptrdiff_t difference_type;
  typedef T* pointer;
  typedef T value_type;
  rt_allocator() {}
  ~rt_allocator() {}
  template <class U> rt_allocator(const rt_allocator<U>&) {}

  pointer allocate(size_type n) {
    return (pointer)RTAlloc(g_pWorld, n * sizeof(T));
  }
  void deallocate(pointer p, size_type n) {
    (void)&n;
    RTFree(g_pWorld, p);
  }
};

#endif
