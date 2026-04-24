// Minimal self-relative pointer for cross-process shared memory.
// Drop-in replacement for boost::interprocess::offset_ptr.
//
// Stores the byte offset from its own address to the target, so it works
// regardless of where each process maps the shared memory segment.

#pragma once

#include <cstddef>
#include <cstdint>

namespace detail_server_shm {

template<typename T>
class relative_ptr {
    // 1 is the null sentinel — a self-relative offset of 0 would mean
    // "pointing at myself" which is a valid (if unusual) state, so we
    // reserve 1 (pointing one byte past ourselves) as "null".
    static constexpr ptrdiff_t NULL_OFFSET = 1;
    ptrdiff_t offset_;

    void set(T* p) noexcept {
        if (p)
            offset_ = reinterpret_cast<char*>(p)
                     - reinterpret_cast<const char*>(&offset_);
        else
            offset_ = NULL_OFFSET;
    }

public:
    relative_ptr() noexcept : offset_(NULL_OFFSET) {}
    relative_ptr(T* p) noexcept { set(p); }
    relative_ptr(const relative_ptr& other) noexcept { set(other.get()); }
    relative_ptr& operator=(const relative_ptr& other) noexcept { set(other.get()); return *this; }
    relative_ptr& operator=(T* p) noexcept { set(p); return *this; }

    T* get() const noexcept {
        if (offset_ == NULL_OFFSET) return nullptr;
        return reinterpret_cast<T*>(
            const_cast<char*>(reinterpret_cast<const char*>(&offset_)) + offset_
        );
    }

    T& operator*() const noexcept { return *get(); }
    T* operator->() const noexcept { return get(); }
    explicit operator bool() const noexcept { return offset_ != NULL_OFFSET; }
    bool operator==(std::nullptr_t) const noexcept { return offset_ == NULL_OFFSET; }
    bool operator!=(std::nullptr_t) const noexcept { return offset_ != NULL_OFFSET; }

    relative_ptr operator+(ptrdiff_t n) const noexcept {
        return relative_ptr(get() + n);
    }
};

} // namespace detail_server_shm
