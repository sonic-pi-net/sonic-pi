//  altivec integer vector helper class
//
//  Copyright (C) 2011 Tim Blechmann
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
//  Boston, MA 02110-1301, USA.

#ifndef VEC_INT_ALTIVEC_HPP
#define VEC_INT_ALTIVEC_HPP

#include <altivec.h>

namespace nova {
namespace detail {

struct int_vec_altivec
{
    typedef __vector float fvec;
    typedef __vector unsigned int ivec;
    ivec data_;

private:
    static ivec set_vector(int i)
    {
#ifdef __GNUC__
        return (ivec){i, i, i, i};
#else
#error compiler not supported
#endif
    }

public:
    explicit int_vec_altivec(int arg):
        data_(set_vector(arg))
    {}

    int_vec_altivec(fvec arg):
        data_((ivec)arg)
    {}

    int_vec_altivec(ivec arg):
        data_(arg)
    {}

    int_vec_altivec(int_vec_altivec const & arg):
        data_(arg.data_)
    {}

    int_vec_altivec(__vector signed int arg):
        data_((ivec)arg)
    {}

    int_vec_altivec(void)
    {}

    operator ivec (void) const
    {
        return data_;
    }

    friend int_vec_altivec operator+(int_vec_altivec const & lhs, int_vec_altivec const & rhs)
    {
        return vec_add(lhs.data_, rhs.data_);
    }

    friend int_vec_altivec operator-(int_vec_altivec const & lhs, int_vec_altivec const & rhs)
    {
        return vec_sub(lhs.data_, rhs.data_);
    }

    #define RELATIONAL_MASK_OPERATOR(op, opcode) \
    friend inline int_vec_altivec mask_##op(int_vec_altivec const & lhs, int_vec_altivec const & rhs) \
    { \
        return int_vec_altivec((ivec)opcode(lhs.data_, rhs.data_)); \
    }

    RELATIONAL_MASK_OPERATOR(lt, vec_cmplt)
    RELATIONAL_MASK_OPERATOR(gt, vec_cmpgt)
    RELATIONAL_MASK_OPERATOR(eq, vec_cmpeq)

    #undef RELATIONAL_MASK_OPERATOR

    friend int_vec_altivec operator&(int_vec_altivec const & lhs, int_vec_altivec const & rhs)
    {
        return int_vec_altivec (vec_and(lhs.data_, rhs.data_));
    }

    friend inline int_vec_altivec andnot(int_vec_altivec const & lhs, int_vec_altivec const & rhs)
    {
        return int_vec_altivec(vec_andc(lhs.data_, rhs.data_));
    }

    // shift in zeros
    friend inline int_vec_altivec slli(int_vec_altivec const & arg, int count)
    {
        return vec_sl(arg.data_, set_vector(count));
    }

    // shift in zeros
    friend inline int_vec_altivec srli(int_vec_altivec const & arg, int count)
    {
        return vec_sr(arg.data_, set_vector(count));
    }

    inline fvec convert_to_float(void) const
    {
        return vec_ctf(data_, 0);
    }
};

}
}

#endif /* VEC_INT_ALTIVEC_HPP */
