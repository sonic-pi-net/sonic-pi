/*
 *    SuperCollider real time audio synthesis system
 *    Copyright (c) 2002 James McCartney. All rights reserved.
 *    Copyright (c) 2011 Tim Blechmann
 *
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#pragma once

#include <cassert>

#include "SC_PlugIn.h"
#include "function_attributes.h"

#include <type_traits>

/// c++ wrapper for Unit struct
class SCUnit : public Unit {
public:
    ///@{
    /// generic signal wrappers
    template <typename FloatType> struct ScalarSignal {
        explicit ScalarSignal(FloatType value): value(value) {}

        FloatType consume() const { return value; }

        FloatType value;
    };

    template <typename FloatType> struct SlopeSignal {
        SlopeSignal(FloatType value, FloatType slope): value(value), slope(slope) {}

        FloatType consume() {
            FloatType ret = value;
            value += slope;
            return ret;
        }

        FloatType value, slope;
    };

    template <typename FloatType> struct AudioSignal {
        explicit AudioSignal(const FloatType* pointer): pointer(pointer) {}

        FloatType consume() { return *pointer++; }

        const FloatType* pointer;
    };

    template <typename FloatType> inline ScalarSignal<FloatType> makeScalar(FloatType value) const {
        return ScalarSignal<FloatType>(value);
    }

    template <typename FloatType> inline SlopeSignal<FloatType> makeSlope(FloatType next, FloatType last) const {
        return SlopeSignal<FloatType>(last, calcSlope(next, last));
    }

    inline AudioSignal<float> makeSignal(int index) const {
        const float* input = in(index);
        return AudioSignal<float>(input);
    }
    ///@}

    /// get input signal at index
    const float* in(int index) const {
        assert(uint32(index) < mNumInputs);
        const Unit* unit = this;
        return IN(index);
    }

    /// get input signal at index (to be used with ZXP)
    const float* zin(int index) const {
        assert(uint32(index) < mNumInputs);
        const Unit* unit = this;
        return ZIN(index);
    }

    /// get first sample of input signal
    float in0(int index) const {
        assert(uint32(index) < mNumInputs);
        const Unit* unit = this;
        return IN0(index);
    }

    /// get output signal at index
    float* out(int index) const {
        assert(uint32(index) < mNumOutputs);
        const Unit* unit = this;
        return OUT(index);
    }

    /// get output signal at index (to be used with ZXP)
    float* zout(int index) const {
        assert(uint32(index) < mNumOutputs);
        const Unit* unit = this;
        return ZOUT(index);
    }

    /// get reference to first sample of output signal
    float& out0(int index) const {
        assert(uint32(index) < mNumOutputs);
        const Unit* unit = this;
        return OUT0(index);
    }

    /// get rate of input signal
    int inRate(int index) const {
        assert(uint32(index) < mNumInputs);
        const Unit* unit = this;
        return INRATE(index);
    }

    /// get number of inputs
    int numInputs() const { return int(mNumInputs); }

    /// get number of outputs
    int numOutputs() const { return int(mNumOutputs); }

    /// test if input signal at index is scalar rate
    bool isScalarRateIn(int index) const {
        assert(uint32(index) < mNumInputs);
        return inRate(index) == calc_ScalarRate;
    }

    /// test if input signal at index is demand rate
    bool isDemandRateIn(int index) const {
        assert(uint32(index) < mNumInputs);
        return inRate(index) == calc_DemandRate;
    }

    /// test if input signal at index is control rate
    bool isControlRateIn(int index) const {
        assert(uint32(index) < mNumInputs);
        return inRate(index) == calc_BufRate;
    }

    /// test if input signal at index is audio rate
    bool isAudioRateIn(int index) const {
        assert(uint32(index) < mNumInputs);
        return inRate(index) == calc_FullRate;
    }

    /// get the blocksize of the input
    int inBufferSize(int index) const {
        assert(uint32(index) < mNumInputs);
        const Unit* unit = this;
        return INBUFLENGTH(index);
    }

    /// get sample rate of ugen
    double sampleRate() const {
        const Unit* unit = this;
        return SAMPLERATE;
    }

    /// get sample duration
    double sampleDur() const {
        const Unit* unit = this;
        return SAMPLEDUR;
    }

    /// get buffer size of ugen
    int bufferSize() const { return mBufLength; }

    /// get control rate
    double controlRate() const {
        const Unit* unit = this;
        return BUFRATE;
    }

    /// get duration of a control block
    double controlDur() const {
        const Unit* unit = this;
        return BUFDUR;
    }

    /// get sampling rate of audio signal
    double fullSampleRate() const {
        const Unit* unit = this;
        return FULLRATE;
    }

    /// get buffer size of audio signals
    int fullBufferSize() const {
        const Unit* unit = this;
        return FULLBUFLENGTH;
    }

    /// calculate slope value
    template <typename FloatType> FloatType calcSlope(FloatType next, FloatType prev) const {
        const Unit* unit = this;
        return CALCSLOPE(next, prev);
    }

    template <typename UnitType, void (UnitType::*PointerToMember)(int)> static UnitCalcFunc make_calc_function(void) {
        return &run_member_function<UnitType, PointerToMember>;
    }

    /// set calc function & compute initial sample
    template <typename UnitType, void (UnitType::*PointerToMember)(int)> void set_calc_function(void) {
        mCalcFunc = make_calc_function<UnitType, PointerToMember>();
        (mCalcFunc)(this, 1);
    }

    /// set calc function & compute initial sample
    template <typename UnitType, void (UnitType::*VectorCalcFunc)(int), void (UnitType::*ScalarCalcFunc)(int)>
    void set_vector_calc_function(void) {
        mCalcFunc = make_calc_function<UnitType, VectorCalcFunc>();
        make_calc_function<UnitType, ScalarCalcFunc>()(this, 1);
    }
    /// @}

private:
    template <typename UnitType, void (UnitType::*PointerToMember)(int)>
    HOT static void run_member_function(struct Unit* unit, int inNumSamples) {
        UnitType* realUnit = static_cast<UnitType*>(unit);
        ((realUnit)->*(PointerToMember))(inNumSamples);
    }
};

/// define Ctor/Dtor functions for a class
#define DEFINE_XTORS(CLASSNAME)                                                                                        \
    void CLASSNAME##_Ctor(CLASSNAME* unit) { new (unit) CLASSNAME(); }                                                 \
                                                                                                                       \
    void CLASSNAME##_Dtor(CLASSNAME* unit) { unit->~CLASSNAME(); }

namespace detail {

template <class UGenClass> void constructClass(Unit* unit) { new (static_cast<UGenClass*>(unit)) UGenClass(); }
template <class UGenClass> void destroyClass(Unit* unit) { static_cast<UGenClass*>(unit)->~UGenClass(); }

}

template <class Unit> void registerUnit(InterfaceTable* ft, const char* name, bool disableBufferAliasing = false) {
    UnitCtorFunc ctor = detail::constructClass<Unit>;
    UnitDtorFunc dtor = std::is_trivially_destructible<Unit>::value ? nullptr : detail::destroyClass<Unit>;

    (*ft->fDefineUnit)(name, sizeof(Unit), ctor, dtor, uint32(disableBufferAliasing ? 1 : 0));
}
