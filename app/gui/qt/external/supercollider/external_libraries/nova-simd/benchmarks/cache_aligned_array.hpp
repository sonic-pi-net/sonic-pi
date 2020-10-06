/* cache-aligned array, based on boost::array
 *
 * The following code declares class array,
 * an STL container (as wrapper) for arrays of constant size.
 *
 * See
 *      http://www.boost.org/libs/array/
 * for documentation.
 *
 * The original author site is at: http://www.josuttis.com/
 *
 * (C) Copyright Nicolai M. Josuttis 2001.
 *
 * Distributed under the Boost Software License, Version 1.0. (See
 * accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *
 * 29 Jan 2004 - c_array() added, BOOST_NO_PRIVATE_IN_AGGREGATE removed (Nico Josuttis)
 * 23 Aug 2002 - fix for Non-MSVC compilers combined with MSVC libraries.
 * 05 Aug 2001 - minor update (Nico Josuttis)
 * 20 Jan 2001 - STLport fix (Beman Dawes)
 * 29 Sep 2000 - Initial Revision (Nico Josuttis)
 *
 * Jan 29, 2004
 */
#ifndef CACHE_ALIGNED_ARRAY_HPP
#define CACHE_ALIGNED_ARRAY_HPP

#include <cstddef>
#include <stdexcept>
#include <boost/assert.hpp>

// Handles broken standard libraries better than <iterator>
#include <boost/detail/iterator.hpp>
#include <boost/throw_exception.hpp>
#include <algorithm>

// FIXES for broken compilers
#include <boost/config.hpp>

#include "malloc_aligned.hpp"

namespace nova {

    template<class T, std::size_t N>
    class aligned_array {
      public:
        T * elems; // fixed-size array of elements of type T

      public:
        aligned_array(void)
        {
            elems = (T*)malloc_aligned(N * sizeof(T));
            for (int i = 0; i != N; ++i)
                new(elems+i) T();
        }

        aligned_array(aligned_array const & rhs)
        {
            elems = (T*)malloc_aligned(N * sizeof(T));
            for (int i = 0; i != N; ++i)
                new(elems+i) T();
            operator=(rhs);
        }

        ~aligned_array(void)
        {
            for (int i = 0; i != N; ++i)
                elems[i].~T();
            free_aligned(elems);
        }

        // type definitions
        typedef T              value_type;
        typedef T*             iterator;
        typedef const T*       const_iterator;
        typedef T&             reference;
        typedef const T&       const_reference;
        typedef std::size_t    size_type;
        typedef std::ptrdiff_t difference_type;

        // iterator support
        iterator begin() { return elems; }
        const_iterator begin() const { return elems; }
        iterator end() { return elems+N; }
        const_iterator end() const { return elems+N; }

        // reverse iterator support
#if !defined(BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION) && !defined(BOOST_MSVC_STD_ITERATOR) && !defined(BOOST_NO_STD_ITERATOR_TRAITS)
        typedef std::reverse_iterator<iterator> reverse_iterator;
        typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
#elif defined(_MSC_VER) && (_MSC_VER == 1300) && defined(BOOST_DINKUMWARE_STDLIB) && (BOOST_DINKUMWARE_STDLIB == 310)
        // workaround for broken reverse_iterator in VC7
        typedef std::reverse_iterator<std::_Ptrit<value_type, difference_type, iterator,
                                      reference, iterator, reference> > reverse_iterator;
        typedef std::reverse_iterator<std::_Ptrit<value_type, difference_type, const_iterator,
                                      const_reference, iterator, reference> > const_reverse_iterator;
#else
        // workaround for broken reverse_iterator implementations
        typedef std::reverse_iterator<iterator,T> reverse_iterator;
        typedef std::reverse_iterator<const_iterator,T> const_reverse_iterator;
#endif

        reverse_iterator rbegin() { return reverse_iterator(end()); }
        const_reverse_iterator rbegin() const {
            return const_reverse_iterator(end());
        }
        reverse_iterator rend() { return reverse_iterator(begin()); }
        const_reverse_iterator rend() const {
            return const_reverse_iterator(begin());
        }

        // operator[]
        reference operator[](size_type i)
        {
            BOOST_ASSERT( i < N && "out of range" );
            return elems[i];
        }

        const_reference operator[](size_type i) const
        {
            BOOST_ASSERT( i < N && "out of range" );
            return elems[i];
        }

        // at() with range check
        reference at(size_type i) { rangecheck(i); return elems[i]; }
        const_reference at(size_type i) const { rangecheck(i); return elems[i]; }

        // front() and back()
        reference front()
        {
            return elems[0];
        }

        const_reference front() const
        {
            return elems[0];
        }

        reference back()
        {
            return elems[N-1];
        }

        const_reference back() const
        {
            return elems[N-1];
        }

        // size is constant
        static size_type size() { return N; }
        static bool empty() { return false; }
        static size_type max_size() { return N; }
        enum { static_size = N };

        // swap (note: linear complexity)
        void swap (aligned_array<T,N>& y) {
            std::swap_ranges(begin(),end(),y.begin());
        }

        // direct access to data (read-only)
        const T* data() const { return elems; }
        T* data() { return elems; }

        // use array as C array (direct read/write access to data)
        T* c_array() { return elems; }

        // assignment with type conversion
        aligned_array & operator=(const aligned_array & rhs)
        {
            std::copy(rhs.begin(),rhs.end(), begin());
            return *this;
        }

        template <typename T2>
        aligned_array<T,N>& operator= (const aligned_array<T2,N>& rhs) {
            std::copy(rhs.begin(),rhs.end(), begin());
            return *this;
        }

        // assign one value to all elements
        void assign (const T& value)
        {
            std::fill_n(begin(),size(),value);
        }

        // check range (may be private because it is static)
        static void rangecheck (size_type i) {
            if (i >= size()) {
                throw std::out_of_range("aligned_array<>: index out of range");
            }
        }

    };

#if !defined(BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION)
    template< class T >
    class aligned_array< T, 0 > {

      public:
        // type definitions
        typedef T              value_type;
        typedef T*             iterator;
        typedef const T*       const_iterator;
        typedef T&             reference;
        typedef const T&       const_reference;
        typedef std::size_t    size_type;
        typedef std::ptrdiff_t difference_type;

        // iterator support
        iterator begin() { return iterator( reinterpret_cast< T * >( this ) ); }
        const_iterator begin() const { return const_iterator(  reinterpret_cast< const T * >( this ) ); }
        iterator end() { return begin(); }
        const_iterator end() const { return begin(); }

        // reverse iterator support
#if !defined(BOOST_NO_TEMPLATE_PARTIAL_SPECIALIZATION) && !defined(BOOST_MSVC_STD_ITERATOR) && !defined(BOOST_NO_STD_ITERATOR_TRAITS)
        typedef std::reverse_iterator<iterator> reverse_iterator;
        typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
#elif defined(_MSC_VER) && (_MSC_VER == 1300) && defined(BOOST_DINKUMWARE_STDLIB) && (BOOST_DINKUMWARE_STDLIB == 310)
        // workaround for broken reverse_iterator in VC7
        typedef std::reverse_iterator<std::_Ptrit<value_type, difference_type, iterator,
                                      reference, iterator, reference> > reverse_iterator;
        typedef std::reverse_iterator<std::_Ptrit<value_type, difference_type, const_iterator,
                                      const_reference, iterator, reference> > const_reverse_iterator;
#else
        // workaround for broken reverse_iterator implementations
        typedef std::reverse_iterator<iterator,T> reverse_iterator;
        typedef std::reverse_iterator<const_iterator,T> const_reverse_iterator;
#endif

        reverse_iterator rbegin() { return reverse_iterator(end()); }
        const_reverse_iterator rbegin() const {
            return const_reverse_iterator(end());
        }
        reverse_iterator rend() { return reverse_iterator(begin()); }
        const_reverse_iterator rend() const {
            return const_reverse_iterator(begin());
        }

        // operator[]
        reference operator[](size_type i)
        {
            return failed_rangecheck();
        }

        const_reference operator[](size_type i) const
        {
            return failed_rangecheck();
        }

        // at() with range check
        reference at(size_type i)               {   return failed_rangecheck(); }
        const_reference at(size_type i) const   {   return failed_rangecheck(); }

        // front() and back()
        reference front()
        {
            return failed_rangecheck();
        }

        const_reference front() const
        {
            return failed_rangecheck();
        }

        reference back()
        {
            return failed_rangecheck();
        }

        const_reference back() const
        {
            return failed_rangecheck();
        }

        // size is constant
        static size_type size() { return 0; }
        static bool empty() { return true; }
        static size_type max_size() { return 0; }
        enum { static_size = 0 };

        void swap (aligned_array<T,0>& y) {
        }

        // direct access to data (read-only)
        const T* data() const { return 0; }
        T* data() { return 0; }

        // use array as C array (direct read/write access to data)
        T* c_array() { return 0; }

        // assignment with type conversion
        template <typename T2>
        aligned_array<T,0>& operator= (const aligned_array<T2,0>& ) {
            return *this;
        }

        // assign one value to all elements
        void assign (const T& ) {   }

        // check range (may be private because it is static)
        static reference failed_rangecheck () {
                std::out_of_range e("attempt to access element of an empty aligned_array");
                boost::throw_exception(e);
                //
                // We need to return something here to keep
                // some compilers happy: however we will never
                // actually get here....
                //
                static T placeholder;
                return placeholder;
            }
    };
#endif

    // comparisons
    template<class T, std::size_t N>
    bool operator== (const aligned_array<T,N>& x, const aligned_array<T,N>& y) {
        return std::equal(x.begin(), x.end(), y.begin());
    }
    template<class T, std::size_t N>
    bool operator< (const aligned_array<T,N>& x, const aligned_array<T,N>& y) {
        return std::lexicographical_compare(x.begin(),x.end(),y.begin(),y.end());
    }
    template<class T, std::size_t N>
    bool operator!= (const aligned_array<T,N>& x, const aligned_array<T,N>& y) {
        return !(x==y);
    }
    template<class T, std::size_t N>
    bool operator> (const aligned_array<T,N>& x, const aligned_array<T,N>& y) {
        return y<x;
    }
    template<class T, std::size_t N>
    bool operator<= (const aligned_array<T,N>& x, const aligned_array<T,N>& y) {
        return !(y<x);
    }
    template<class T, std::size_t N>
    bool operator>= (const aligned_array<T,N>& x, const aligned_array<T,N>& y) {
        return !(x<y);
    }

    // global swap()
    template<class T, std::size_t N>
    inline void swap (aligned_array<T,N>& x, aligned_array<T,N>& y) {
        x.swap(y);
    }

template <typename sample_type, unsigned int n>
#ifdef GCC
inline sample_type __attribute__((aligned(64))) * get_samples(aligned_array<sample_type, n> & arg)
#else
inline sample_type * get_samples(aligned_array<sample_type, n> & arg)
#endif
{
    return arg.begin();
}

template <typename sample_type, unsigned int n>
#ifdef GCC
inline const sample_type __attribute__((aligned(64))) * get_samples(aligned_array<sample_type, n> const & arg)
#else
inline const sample_type * get_samples(aligned_array<sample_type, n> const & arg)
#endif
{
    return arg.begin();
}


} /* namespace nova */

#endif /*CACHE_ALIGNED_ARRAY_HPP*/
