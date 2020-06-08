/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   JUCE is an open source library subject to commercial or open-source
   licensing.

   The code included in this file is provided under the terms of the ISC license
   http://www.isc.org/downloads/software-support-policy/isc-license. Permission
   To use, copy, modify, and/or distribute this software for any purpose with or
   without fee is hereby granted provided that the above copyright notice and
   this permission notice appear in all copies.

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

namespace
{
    inline uint32 bitToMask  (const int bit) noexcept           { return (uint32) 1 << (bit & 31); }
    inline size_t bitToIndex (const int bit) noexcept           { return (size_t) (bit >> 5); }
    inline size_t sizeNeededToHold (int highestBit) noexcept    { return (size_t) (highestBit >> 5) + 1; }
}

int findHighestSetBit (uint32 n) noexcept
{
    jassert (n != 0); // (the built-in functions may not work for n = 0)

  #if JUCE_GCC || JUCE_CLANG
    return 31 - __builtin_clz (n);
  #elif JUCE_MSVC
    unsigned long highest;
    _BitScanReverse (&highest, n);
    return (int) highest;
  #else
    n |= (n >> 1);
    n |= (n >> 2);
    n |= (n >> 4);
    n |= (n >> 8);
    n |= (n >> 16);
    return countNumberOfBits (n >> 1);
  #endif
}

//==============================================================================
BigInteger::BigInteger()
    : allocatedSize (numPreallocatedInts)
{
    for (int i = 0; i < numPreallocatedInts; ++i)
        preallocated[i] = 0;
}

BigInteger::BigInteger (const int32 value)
    : allocatedSize (numPreallocatedInts),
      highestBit (31),
      negative (value < 0)
{
    preallocated[0] = (uint32) std::abs (value);

    for (int i = 1; i < numPreallocatedInts; ++i)
        preallocated[i] = 0;

    highestBit = getHighestBit();
}

BigInteger::BigInteger (const uint32 value)
    : allocatedSize (numPreallocatedInts),
      highestBit (31)
{
    preallocated[0] = value;

    for (int i = 1; i < numPreallocatedInts; ++i)
        preallocated[i] = 0;

    highestBit = getHighestBit();
}

BigInteger::BigInteger (int64 value)
    : allocatedSize (numPreallocatedInts),
      highestBit (63),
      negative (value < 0)
{
    if (value < 0)
        value = -value;

    preallocated[0] = (uint32) value;
    preallocated[1] = (uint32) (value >> 32);

    for (int i = 2; i < numPreallocatedInts; ++i)
        preallocated[i] = 0;

    highestBit = getHighestBit();
}

BigInteger::BigInteger (const BigInteger& other)
    : allocatedSize (other.allocatedSize),
      highestBit (other.getHighestBit()),
      negative (other.negative)
{
    if (allocatedSize > numPreallocatedInts)
        heapAllocation.malloc (allocatedSize);

    memcpy (getValues(), other.getValues(), sizeof (uint32) * allocatedSize);
}

BigInteger::BigInteger (BigInteger&& other) noexcept
    : heapAllocation (std::move (other.heapAllocation)),
      allocatedSize (other.allocatedSize),
      highestBit (other.highestBit),
      negative (other.negative)
{
    memcpy (preallocated, other.preallocated, sizeof (preallocated));
}

BigInteger& BigInteger::operator= (BigInteger&& other) noexcept
{
    heapAllocation = std::move (other.heapAllocation);
    memcpy (preallocated, other.preallocated, sizeof (preallocated));
    allocatedSize = other.allocatedSize;
    highestBit = other.highestBit;
    negative = other.negative;
    return *this;
}

BigInteger::~BigInteger()
{
}

void BigInteger::swapWith (BigInteger& other) noexcept
{
    for (int i = 0; i < numPreallocatedInts; ++i)
        std::swap (preallocated[i], other.preallocated[i]);

    heapAllocation.swapWith (other.heapAllocation);
    std::swap (allocatedSize, other.allocatedSize);
    std::swap (highestBit, other.highestBit);
    std::swap (negative, other.negative);
}

BigInteger& BigInteger::operator= (const BigInteger& other)
{
    if (this != &other)
    {
        highestBit = other.getHighestBit();
        auto newAllocatedSize = (size_t) jmax ((size_t) numPreallocatedInts, sizeNeededToHold (highestBit));

        if (newAllocatedSize <= numPreallocatedInts)
            heapAllocation.free();
        else if (newAllocatedSize != allocatedSize)
            heapAllocation.malloc (newAllocatedSize);

        allocatedSize = newAllocatedSize;

        memcpy (getValues(), other.getValues(), sizeof (uint32) * allocatedSize);
        negative = other.negative;
    }

    return *this;
}

uint32* BigInteger::getValues() const noexcept
{
    jassert (heapAllocation != nullptr || allocatedSize <= numPreallocatedInts);

    return heapAllocation != nullptr ? heapAllocation
                                     : const_cast<uint32*> (preallocated);
}

uint32* BigInteger::ensureSize (const size_t numVals)
{
    if (numVals > allocatedSize)
    {
        auto oldSize = allocatedSize;
        allocatedSize = ((numVals + 2) * 3) / 2;

        if (heapAllocation == nullptr)
        {
            heapAllocation.calloc (allocatedSize);
            memcpy (heapAllocation, preallocated, sizeof (uint32) * numPreallocatedInts);
        }
        else
        {
            heapAllocation.realloc (allocatedSize);

            for (auto* values = getValues(); oldSize < allocatedSize; ++oldSize)
                values[oldSize] = 0;
        }
    }

    return getValues();
}

//==============================================================================
bool BigInteger::operator[] (const int bit) const noexcept
{
    return bit <= highestBit && bit >= 0
             && ((getValues() [bitToIndex (bit)] & bitToMask (bit)) != 0);
}

int BigInteger::toInteger() const noexcept
{
    auto n = (int) (getValues()[0] & 0x7fffffff);
    return negative ? -n : n;
}

int64 BigInteger::toInt64() const noexcept
{
    auto* values = getValues();
    auto n = (((int64) (values[1] & 0x7fffffff)) << 32) | values[0];
    return negative ? -n : n;
}

BigInteger BigInteger::getBitRange (int startBit, int numBits) const
{
    BigInteger r;
    numBits = jmax (0, jmin (numBits, getHighestBit() + 1 - startBit));
    auto* destValues = r.ensureSize (sizeNeededToHold (numBits));
    r.highestBit = numBits;

    for (int i = 0; numBits > 0;)
    {
        destValues[i++] = getBitRangeAsInt (startBit, (int) jmin (32, numBits));
        numBits -= 32;
        startBit += 32;
    }

    r.highestBit = r.getHighestBit();
    return r;
}

uint32 BigInteger::getBitRangeAsInt (const int startBit, int numBits) const noexcept
{
    if (numBits > 32)
    {
        jassertfalse;  // use getBitRange() if you need more than 32 bits..
        numBits = 32;
    }

    numBits = jmin (numBits, highestBit + 1 - startBit);

    if (numBits <= 0)
        return 0;

    auto pos = bitToIndex (startBit);
    auto offset = startBit & 31;
    auto endSpace = 32 - numBits;
    auto* values = getValues();

    auto n = ((uint32) values [pos]) >> offset;

    if (offset > endSpace)
        n |= ((uint32) values [pos + 1]) << (32 - offset);

    return n & (((uint32) 0xffffffff) >> endSpace);
}

void BigInteger::setBitRangeAsInt (const int startBit, int numBits, uint32 valueToSet)
{
    if (numBits > 32)
    {
        jassertfalse;
        numBits = 32;
    }

    for (int i = 0; i < numBits; ++i)
    {
        setBit (startBit + i, (valueToSet & 1) != 0);
        valueToSet >>= 1;
    }
}

//==============================================================================
void BigInteger::clear() noexcept
{
    heapAllocation.free();
    allocatedSize = numPreallocatedInts;
    highestBit = -1;
    negative = false;

    for (int i = 0; i < numPreallocatedInts; ++i)
        preallocated[i] = 0;
}

void BigInteger::setBit (const int bit)
{
    if (bit >= 0)
    {
        if (bit > highestBit)
        {
            ensureSize (sizeNeededToHold (bit));
            highestBit = bit;
        }

        getValues() [bitToIndex (bit)] |= bitToMask (bit);
    }
}

void BigInteger::setBit (const int bit, const bool shouldBeSet)
{
    if (shouldBeSet)
        setBit (bit);
    else
        clearBit (bit);
}

void BigInteger::clearBit (const int bit) noexcept
{
    if (bit >= 0 && bit <= highestBit)
    {
        getValues() [bitToIndex (bit)] &= ~bitToMask (bit);

        if (bit == highestBit)
            highestBit = getHighestBit();
    }
}

void BigInteger::setRange (int startBit, int numBits, const bool shouldBeSet)
{
    while (--numBits >= 0)
        setBit (startBit++, shouldBeSet);
}

void BigInteger::insertBit (const int bit, const bool shouldBeSet)
{
    if (bit >= 0)
        shiftBits (1, bit);

    setBit (bit, shouldBeSet);
}

//==============================================================================
bool BigInteger::isZero() const noexcept
{
    return getHighestBit() < 0;
}

bool BigInteger::isOne() const noexcept
{
    return getHighestBit() == 0 && ! negative;
}

bool BigInteger::isNegative() const noexcept
{
    return negative && ! isZero();
}

void BigInteger::setNegative (const bool neg) noexcept
{
    negative = neg;
}

void BigInteger::negate() noexcept
{
    negative = (! negative) && ! isZero();
}

#if JUCE_MSVC && ! defined (__INTEL_COMPILER)
 #pragma intrinsic (_BitScanReverse)
#endif

int BigInteger::countNumberOfSetBits() const noexcept
{
    int total = 0;
    auto* values = getValues();

    for (int i = (int) sizeNeededToHold (highestBit); --i >= 0;)
        total += countNumberOfBits (values[i]);

    return total;
}

int BigInteger::getHighestBit() const noexcept
{
    auto* values = getValues();

    for (int i = (int) bitToIndex (highestBit); i >= 0; --i)
        if (uint32 n = values[i])
            return findHighestSetBit (n) + (i << 5);

    return -1;
}

int BigInteger::findNextSetBit (int i) const noexcept
{
    auto* values = getValues();

    for (; i <= highestBit; ++i)
        if ((values [bitToIndex (i)] & bitToMask (i)) != 0)
            return i;

    return -1;
}

int BigInteger::findNextClearBit (int i) const noexcept
{
    auto* values = getValues();

    for (; i <= highestBit; ++i)
        if ((values [bitToIndex (i)] & bitToMask (i)) == 0)
            break;

    return i;
}

//==============================================================================
BigInteger& BigInteger::operator+= (const BigInteger& other)
{
    if (this == &other)
        return operator+= (BigInteger (other));

    if (other.isNegative())
        return operator-= (-other);

    if (isNegative())
    {
        if (compareAbsolute (other) < 0)
        {
            auto temp = *this;
            temp.negate();
            *this = other;
            *this -= temp;
        }
        else
        {
            negate();
            *this -= other;
            negate();
        }
    }
    else
    {
        highestBit = jmax (highestBit, other.highestBit) + 1;

        auto numInts = sizeNeededToHold (highestBit);
        auto* values = ensureSize (numInts);
        auto* otherValues = other.getValues();
        int64 remainder = 0;

        for (size_t i = 0; i < numInts; ++i)
        {
            remainder += values[i];

            if (i < other.allocatedSize)
                remainder += otherValues[i];

            values[i] = (uint32) remainder;
            remainder >>= 32;
        }

        jassert (remainder == 0);
        highestBit = getHighestBit();
    }

    return *this;
}

BigInteger& BigInteger::operator-= (const BigInteger& other)
{
    if (this == &other)
    {
        clear();
        return *this;
    }

    if (other.isNegative())
        return operator+= (-other);

    if (isNegative())
    {
        negate();
        *this += other;
        negate();
        return *this;
    }

    if (compareAbsolute (other) < 0)
    {
        auto temp = other;
        swapWith (temp);
        *this -= temp;
        negate();
        return *this;
    }

    auto numInts = sizeNeededToHold (getHighestBit());
    auto maxOtherInts = sizeNeededToHold (other.getHighestBit());
    jassert (numInts >= maxOtherInts);
    auto* values = getValues();
    auto* otherValues = other.getValues();
    int64 amountToSubtract = 0;

    for (size_t i = 0; i < numInts; ++i)
    {
        if (i < maxOtherInts)
            amountToSubtract += (int64) otherValues[i];

        if (values[i] >= amountToSubtract)
        {
            values[i] = (uint32) (values[i] - amountToSubtract);
            amountToSubtract = 0;
        }
        else
        {
            const int64 n = ((int64) values[i] + (((int64) 1) << 32)) - amountToSubtract;
            values[i] = (uint32) n;
            amountToSubtract = 1;
        }
    }

    highestBit = getHighestBit();
    return *this;
}

BigInteger& BigInteger::operator*= (const BigInteger& other)
{
    if (this == &other)
        return operator*= (BigInteger (other));

    auto n = getHighestBit();
    auto t = other.getHighestBit();

    auto wasNegative = isNegative();
    setNegative (false);

    BigInteger total;
    total.highestBit = n + t + 1;
    auto* totalValues = total.ensureSize (sizeNeededToHold (total.highestBit) + 1);

    n >>= 5;
    t >>= 5;

    auto m = other;
    m.setNegative (false);

    auto* mValues = m.getValues();
    auto* values = getValues();

    for (int i = 0; i <= t; ++i)
    {
        uint32 c = 0;

        for (int j = 0; j <= n; ++j)
        {
            auto uv = (uint64) totalValues[i + j] + (uint64) values[j] * (uint64) mValues[i] + (uint64) c;
            totalValues[i + j] = (uint32) uv;
            c = uv >> 32;
        }

        totalValues[i + n + 1] = c;
    }

    total.highestBit = total.getHighestBit();
    total.setNegative (wasNegative ^ other.isNegative());
    swapWith (total);

    return *this;
}

void BigInteger::divideBy (const BigInteger& divisor, BigInteger& remainder)
{
    if (this == &divisor)
        return divideBy (BigInteger (divisor), remainder);

    jassert (this != &remainder); // (can't handle passing itself in to get the remainder)

    auto divHB = divisor.getHighestBit();
    auto ourHB = getHighestBit();

    if (divHB < 0 || ourHB < 0)
    {
        // division by zero
        remainder.clear();
        clear();
    }
    else
    {
        auto wasNegative = isNegative();

        swapWith (remainder);
        remainder.setNegative (false);
        clear();

        BigInteger temp (divisor);
        temp.setNegative (false);

        auto leftShift = ourHB - divHB;
        temp <<= leftShift;

        while (leftShift >= 0)
        {
            if (remainder.compareAbsolute (temp) >= 0)
            {
                remainder -= temp;
                setBit (leftShift);
            }

            if (--leftShift >= 0)
                temp >>= 1;
        }

        negative = wasNegative ^ divisor.isNegative();
        remainder.setNegative (wasNegative);
    }
}

BigInteger& BigInteger::operator/= (const BigInteger& other)
{
    BigInteger remainder;
    divideBy (other, remainder);
    return *this;
}

BigInteger& BigInteger::operator|= (const BigInteger& other)
{
    if (this == &other)
        return *this;

    // this operation doesn't take into account negative values..
    jassert (isNegative() == other.isNegative());

    if (other.highestBit >= 0)
    {
        auto* values = ensureSize (sizeNeededToHold (other.highestBit));
        auto* otherValues = other.getValues();

        auto n = (int) bitToIndex (other.highestBit) + 1;

        while (--n >= 0)
            values[n] |= otherValues[n];

        if (other.highestBit > highestBit)
            highestBit = other.highestBit;

        highestBit = getHighestBit();
    }

    return *this;
}

BigInteger& BigInteger::operator&= (const BigInteger& other)
{
    if (this == &other)
        return *this;

    // this operation doesn't take into account negative values..
    jassert (isNegative() == other.isNegative());

    auto* values = getValues();
    auto* otherValues = other.getValues();

    auto n = (int) allocatedSize;

    while (n > (int) other.allocatedSize)
        values[--n] = 0;

    while (--n >= 0)
        values[n] &= otherValues[n];

    if (other.highestBit < highestBit)
        highestBit = other.highestBit;

    highestBit = getHighestBit();
    return *this;
}

BigInteger& BigInteger::operator^= (const BigInteger& other)
{
    if (this == &other)
    {
        clear();
        return *this;
    }

    // this operation will only work with the absolute values
    jassert (isNegative() == other.isNegative());

    if (other.highestBit >= 0)
    {
        auto* values = ensureSize (sizeNeededToHold (other.highestBit));
        auto* otherValues = other.getValues();

        auto n = (int) bitToIndex (other.highestBit) + 1;

        while (--n >= 0)
            values[n] ^= otherValues[n];

        if (other.highestBit > highestBit)
            highestBit = other.highestBit;

        highestBit = getHighestBit();
    }

    return *this;
}

BigInteger& BigInteger::operator%= (const BigInteger& divisor)
{
    BigInteger remainder;
    divideBy (divisor, remainder);
    swapWith (remainder);
    return *this;
}

BigInteger& BigInteger::operator++()      { return operator+= (1); }
BigInteger& BigInteger::operator--()      { return operator-= (1); }
BigInteger  BigInteger::operator++ (int)  { const auto old (*this); operator+= (1); return old; }
BigInteger  BigInteger::operator-- (int)  { const auto old (*this); operator-= (1); return old; }

BigInteger  BigInteger::operator-() const                            { auto b (*this); b.negate(); return b; }
BigInteger  BigInteger::operator+   (const BigInteger& other) const  { auto b (*this); return b += other; }
BigInteger  BigInteger::operator-   (const BigInteger& other) const  { auto b (*this); return b -= other; }
BigInteger  BigInteger::operator*   (const BigInteger& other) const  { auto b (*this); return b *= other; }
BigInteger  BigInteger::operator/   (const BigInteger& other) const  { auto b (*this); return b /= other; }
BigInteger  BigInteger::operator|   (const BigInteger& other) const  { auto b (*this); return b |= other; }
BigInteger  BigInteger::operator&   (const BigInteger& other) const  { auto b (*this); return b &= other; }
BigInteger  BigInteger::operator^   (const BigInteger& other) const  { auto b (*this); return b ^= other; }
BigInteger  BigInteger::operator%   (const BigInteger& other) const  { auto b (*this); return b %= other; }
BigInteger  BigInteger::operator<<  (const int numBits) const        { auto b (*this); return b <<= numBits; }
BigInteger  BigInteger::operator>>  (const int numBits) const        { auto b (*this); return b >>= numBits; }
BigInteger& BigInteger::operator<<= (const int numBits)              { shiftBits (numBits, 0);  return *this; }
BigInteger& BigInteger::operator>>= (const int numBits)              { shiftBits (-numBits, 0); return *this; }

//==============================================================================
int BigInteger::compare (const BigInteger& other) const noexcept
{
    auto isNeg = isNegative();

    if (isNeg == other.isNegative())
    {
        auto absComp = compareAbsolute (other);
        return isNeg ? -absComp : absComp;
    }

    return isNeg ? -1 : 1;
}

int BigInteger::compareAbsolute (const BigInteger& other) const noexcept
{
    auto h1 = getHighestBit();
    auto h2 = other.getHighestBit();

    if (h1 > h2) return 1;
    if (h1 < h2) return -1;

    auto* values = getValues();
    auto* otherValues = other.getValues();

    for (int i = (int) bitToIndex (h1); i >= 0; --i)
        if (values[i] != otherValues[i])
            return values[i] > otherValues[i] ? 1 : -1;

    return 0;
}

bool BigInteger::operator== (const BigInteger& other) const noexcept    { return compare (other) == 0; }
bool BigInteger::operator!= (const BigInteger& other) const noexcept    { return compare (other) != 0; }
bool BigInteger::operator<  (const BigInteger& other) const noexcept    { return compare (other) <  0; }
bool BigInteger::operator<= (const BigInteger& other) const noexcept    { return compare (other) <= 0; }
bool BigInteger::operator>  (const BigInteger& other) const noexcept    { return compare (other) >  0; }
bool BigInteger::operator>= (const BigInteger& other) const noexcept    { return compare (other) >= 0; }

//==============================================================================
void BigInteger::shiftLeft (int bits, const int startBit)
{
    if (startBit > 0)
    {
        for (int i = highestBit; i >= startBit; --i)
            setBit (i + bits, (*this) [i]);

        while (--bits >= 0)
            clearBit (bits + startBit);
    }
    else
    {
        auto* values = ensureSize (sizeNeededToHold (highestBit + bits));
        auto wordsToMove = bitToIndex (bits);
        auto numOriginalInts = bitToIndex (highestBit);
        highestBit += bits;

        if (wordsToMove > 0)
        {
            for (int i = (int) numOriginalInts; i >= 0; --i)
                values[(size_t) i + wordsToMove] = values[i];

            for (size_t j = 0; j < wordsToMove; ++j)
                values[j] = 0;

            bits &= 31;
        }

        if (bits != 0)
        {
            auto invBits = 32 - bits;

            for (size_t i = bitToIndex (highestBit); i > wordsToMove; --i)
                values[i] = (values[i] << bits) | (values[i - 1] >> invBits);

            values[wordsToMove] = values[wordsToMove] << bits;
        }

        highestBit = getHighestBit();
    }
}

void BigInteger::shiftRight (int bits, const int startBit)
{
    if (startBit > 0)
    {
        for (int i = startBit; i <= highestBit; ++i)
            setBit (i, (*this) [i + bits]);

        highestBit = getHighestBit();
    }
    else
    {
        if (bits > highestBit)
        {
            clear();
        }
        else
        {
            auto wordsToMove = bitToIndex (bits);
            auto top = 1 + bitToIndex (highestBit) - wordsToMove;
            highestBit -= bits;
            auto* values = getValues();

            if (wordsToMove > 0)
            {
                for (size_t i = 0; i < top; ++i)
                    values[i] = values[i + wordsToMove];

                for (size_t i = 0; i < wordsToMove; ++i)
                    values[top + i] = 0;

                bits &= 31;
            }

            if (bits != 0)
            {
                auto invBits = 32 - bits;
                --top;

                for (size_t i = 0; i < top; ++i)
                    values[i] = (values[i] >> bits) | (values[i + 1] << invBits);

                values[top] = (values[top] >> bits);
            }

            highestBit = getHighestBit();
        }
    }
}

void BigInteger::shiftBits (int bits, const int startBit)
{
    if (highestBit >= 0)
    {
        if (bits < 0)
            shiftRight (-bits, startBit);
        else if (bits > 0)
            shiftLeft (bits, startBit);
    }
}

//==============================================================================
static BigInteger simpleGCD (BigInteger* m, BigInteger* n)
{
    while (! m->isZero())
    {
        if (n->compareAbsolute (*m) > 0)
            std::swap (m, n);

        *m -= *n;
    }

    return *n;
}

BigInteger BigInteger::findGreatestCommonDivisor (BigInteger n) const
{
    auto m = *this;

    while (! n.isZero())
    {
        if (std::abs (m.getHighestBit() - n.getHighestBit()) <= 16)
            return simpleGCD (&m, &n);

        BigInteger temp2;
        m.divideBy (n, temp2);

        m.swapWith (n);
        n.swapWith (temp2);
    }

    return m;
}

void BigInteger::exponentModulo (const BigInteger& exponent, const BigInteger& modulus)
{
    *this %= modulus;
    auto exp = exponent;
    exp %= modulus;

    if (modulus.getHighestBit() <= 32 || modulus % 2 == 0)
    {
        auto a = *this;
        auto n = exp.getHighestBit();

        for (int i = n; --i >= 0;)
        {
            *this *= *this;

            if (exp[i])
                *this *= a;

            if (compareAbsolute (modulus) >= 0)
                *this %= modulus;
        }
    }
    else
    {
        auto Rfactor = modulus.getHighestBit() + 1;
        BigInteger R (1);
        R.shiftLeft (Rfactor, 0);

        BigInteger R1, m1, g;
        g.extendedEuclidean (modulus, R, m1, R1);

        if (! g.isOne())
        {
            BigInteger a (*this);

            for (int i = exp.getHighestBit(); --i >= 0;)
            {
                *this *= *this;

                if (exp[i])
                    *this *= a;

                if (compareAbsolute (modulus) >= 0)
                    *this %= modulus;
            }
        }
        else
        {
            auto am  = (*this * R) % modulus;
            auto xm = am;
            auto um = R % modulus;

            for (int i = exp.getHighestBit(); --i >= 0;)
            {
                xm.montgomeryMultiplication (xm, modulus, m1, Rfactor);

                if (exp[i])
                    xm.montgomeryMultiplication (am, modulus, m1, Rfactor);
            }

            xm.montgomeryMultiplication (1, modulus, m1, Rfactor);
            swapWith (xm);
        }
    }
}

void BigInteger::montgomeryMultiplication (const BigInteger& other, const BigInteger& modulus,
                                           const BigInteger& modulusp, const int k)
{
    *this *= other;
    auto t = *this;

    setRange (k, highestBit - k + 1, false);
    *this *= modulusp;

    setRange (k, highestBit - k + 1, false);
    *this *= modulus;
    *this += t;
    shiftRight (k, 0);

    if (compare (modulus) >= 0)
        *this -= modulus;
    else if (isNegative())
        *this += modulus;
}

void BigInteger::extendedEuclidean (const BigInteger& a, const BigInteger& b,
                                    BigInteger& x, BigInteger& y)
{
    BigInteger p(a), q(b), gcd(1);
    Array<BigInteger> tempValues;

    while (! q.isZero())
    {
        tempValues.add (p / q);
        gcd = q;
        q = p % q;
        p = gcd;
    }

    x.clear();
    y = 1;

    for (int i = 1; i < tempValues.size(); ++i)
    {
        auto& v = tempValues.getReference (tempValues.size() - i - 1);

        if ((i & 1) != 0)
            x += y * v;
        else
            y += x * v;
    }

    if (gcd.compareAbsolute (y * b - x * a) != 0)
    {
        x.negate();
        x.swapWith (y);
        x.negate();
    }

    swapWith (gcd);
}

void BigInteger::inverseModulo (const BigInteger& modulus)
{
    if (modulus.isOne() || modulus.isNegative())
    {
        clear();
        return;
    }

    if (isNegative() || compareAbsolute (modulus) >= 0)
        *this %= modulus;

    if (isOne())
        return;

    if (findGreatestCommonDivisor (modulus) != 1)
    {
        clear();  // not invertible!
        return;
    }

    BigInteger a1 (modulus), a2 (*this),
               b1 (modulus), b2 (1);

    while (! a2.isOne())
    {
        BigInteger temp1, multiplier (a1);
        multiplier.divideBy (a2, temp1);

        temp1 = a2;
        temp1 *= multiplier;
        auto temp2 = a1;
        temp2 -= temp1;
        a1 = a2;
        a2 = temp2;

        temp1 = b2;
        temp1 *= multiplier;
        temp2 = b1;
        temp2 -= temp1;
        b1 = b2;
        b2 = temp2;
    }

    while (b2.isNegative())
        b2 += modulus;

    b2 %= modulus;
    swapWith (b2);
}

//==============================================================================
OutputStream& JUCE_CALLTYPE operator<< (OutputStream& stream, const BigInteger& value)
{
    return stream << value.toString (10);
}

String BigInteger::toString (const int base, const int minimumNumCharacters) const
{
    String s;
    auto v = *this;

    if (base == 2 || base == 8 || base == 16)
    {
        auto bits = (base == 2) ? 1 : (base == 8 ? 3 : 4);
        static const char hexDigits[] = "0123456789abcdef";

        for (;;)
        {
            auto remainder = v.getBitRangeAsInt (0, bits);
            v >>= bits;

            if (remainder == 0 && v.isZero())
                break;

            s = String::charToString ((juce_wchar) (uint8) hexDigits [remainder]) + s;
        }
    }
    else if (base == 10)
    {
        const BigInteger ten (10);
        BigInteger remainder;

        for (;;)
        {
            v.divideBy (ten, remainder);

            if (remainder.isZero() && v.isZero())
                break;

            s = String (remainder.getBitRangeAsInt (0, 8)) + s;
        }
    }
    else
    {
        jassertfalse; // can't do the specified base!
        return {};
    }

    s = s.paddedLeft ('0', minimumNumCharacters);

    return isNegative() ? "-" + s : s;
}

void BigInteger::parseString (StringRef text, const int base)
{
    clear();
    auto t = text.text.findEndOfWhitespace();

    setNegative (*t == (juce_wchar) '-');

    if (base == 2 || base == 8 || base == 16)
    {
        auto bits = (base == 2) ? 1 : (base == 8 ? 3 : 4);

        for (;;)
        {
            auto c = t.getAndAdvance();
            auto digit = CharacterFunctions::getHexDigitValue (c);

            if (((uint32) digit) < (uint32) base)
            {
                *this <<= bits;
                *this += digit;
            }
            else if (c == 0)
            {
                break;
            }
        }
    }
    else if (base == 10)
    {
        const BigInteger ten ((uint32) 10);

        for (;;)
        {
            auto c = t.getAndAdvance();

            if (c >= '0' && c <= '9')
            {
                *this *= ten;
                *this += (int) (c - '0');
            }
            else if (c == 0)
            {
                break;
            }
        }
    }
}

MemoryBlock BigInteger::toMemoryBlock() const
{
    auto numBytes = (getHighestBit() + 8) >> 3;
    MemoryBlock mb ((size_t) numBytes);
    auto* values = getValues();

    for (int i = 0; i < numBytes; ++i)
        mb[i] = (char) ((values[i / 4] >> ((i & 3) * 8)) & 0xff);

    return mb;
}

void BigInteger::loadFromMemoryBlock (const MemoryBlock& data)
{
    auto numBytes = data.getSize();
    auto numInts = 1 + (numBytes / sizeof (uint32));
    auto* values = ensureSize (numInts);

    for (int i = 0; i < (int) numInts - 1; ++i)
        values[i] = (uint32) ByteOrder::littleEndianInt (addBytesToPointer (data.getData(), (size_t) i * sizeof (uint32)));

    values[numInts - 1] = 0;

    for (int i = (int) (numBytes & ~3u); i < (int) numBytes; ++i)
        this->setBitRangeAsInt (i << 3, 8, (uint32) data [i]);

    highestBit = (int) numBytes * 8;
    highestBit = getHighestBit();
}

//==============================================================================
void writeLittleEndianBitsInBuffer (void* buffer, uint32 startBit, uint32 numBits, uint32 value) noexcept
{
    jassert (buffer != nullptr);
    jassert (numBits > 0 && numBits <= 32);
    jassert (numBits == 32 || (value >> numBits) == 0);

    uint8* data = static_cast<uint8*> (buffer) + startBit / 8;

    if (const uint32 offset = (startBit & 7))
    {
        const uint32 bitsInByte = 8 - offset;
        const uint8 current = *data;

        if (bitsInByte >= numBits)
        {
            *data = (uint8) ((current & ~(((1u << numBits) - 1u) << offset)) | (value << offset));
            return;
        }

        *data++ = current ^ (uint8) (((value << offset) ^ current) & (((1u << bitsInByte) - 1u) << offset));
        numBits -= bitsInByte;
        value >>= bitsInByte;
    }

    while (numBits >= 8)
    {
        *data++ = (uint8) value;
        value >>= 8;
        numBits -= 8;
    }

    if (numBits > 0)
        *data = (uint8) ((*data & (uint32) (0xff << numBits)) | value);
}

uint32 readLittleEndianBitsInBuffer (const void* buffer, uint32 startBit, uint32 numBits) noexcept
{
    jassert (buffer != nullptr);
    jassert (numBits > 0 && numBits <= 32);

    uint32 result = 0;
    uint32 bitsRead = 0;
    const uint8* data = static_cast<const uint8*> (buffer) + startBit / 8;

    if (const uint32 offset = (startBit & 7))
    {
        const uint32 bitsInByte = 8 - offset;
        result = (uint32) (*data >> offset);

        if (bitsInByte >= numBits)
            return result & ((1u << numBits) - 1u);

        numBits -= bitsInByte;
        bitsRead += bitsInByte;
        ++data;
    }

    while (numBits >= 8)
    {
        result |= (((uint32) *data++) << bitsRead);
        bitsRead += 8;
        numBits -= 8;
    }

    if (numBits > 0)
        result |= ((*data & ((1u << numBits) - 1u)) << bitsRead);

    return result;
}


//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

class BigIntegerTests  : public UnitTest
{
public:
    BigIntegerTests()
        : UnitTest ("BigInteger", UnitTestCategories::maths)
    {}

    static BigInteger getBigRandom (Random& r)
    {
        BigInteger b;

        while (b < 2)
            r.fillBitsRandomly (b, 0, r.nextInt (150) + 1);

        return b;
    }

    void runTest() override
    {
        {
            beginTest ("BigInteger");

            Random r = getRandom();

            expect (BigInteger().isZero());
            expect (BigInteger(1).isOne());

            for (int j = 10000; --j >= 0;)
            {
                BigInteger b1 (getBigRandom(r)),
                           b2 (getBigRandom(r));

                BigInteger b3 = b1 + b2;
                expect (b3 > b1 && b3 > b2);
                expect (b3 - b1 == b2);
                expect (b3 - b2 == b1);

                BigInteger b4 = b1 * b2;
                expect (b4 > b1 && b4 > b2);
                expect (b4 / b1 == b2);
                expect (b4 / b2 == b1);
                expect (((b4 << 1) >> 1) == b4);
                expect (((b4 << 10) >> 10) == b4);
                expect (((b4 << 100) >> 100) == b4);

                // TODO: should add tests for other ops (although they also get pretty well tested in the RSA unit test)

                BigInteger b5;
                b5.loadFromMemoryBlock (b3.toMemoryBlock());
                expect (b3 == b5);
            }
        }

        {
            beginTest ("Bit setting");

            Random r = getRandom();
            static uint8 test[2048];

            for (int j = 100000; --j >= 0;)
            {
                uint32 offset = static_cast<uint32> (r.nextInt (200) + 10);
                uint32 num = static_cast<uint32> (r.nextInt (32) + 1);
                uint32 value = static_cast<uint32> (r.nextInt());

                if (num < 32)
                    value &= ((1u << num) - 1);

                auto old1 = readLittleEndianBitsInBuffer (test, offset - 6, 6);
                auto old2 = readLittleEndianBitsInBuffer (test, offset + num, 6);
                writeLittleEndianBitsInBuffer (test, offset, num, value);
                auto result = readLittleEndianBitsInBuffer (test, offset, num);

                expect (result == value);
                expect (old1 == readLittleEndianBitsInBuffer (test, offset - 6, 6));
                expect (old2 == readLittleEndianBitsInBuffer (test, offset + num, 6));
            }
        }
    }
};

static BigIntegerTests bigIntegerTests;

#endif

} // namespace juce
