/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

   Permission is granted to use this software under the terms of the ISC license
   http://www.isc.org/downloads/software-support-policy/isc-license/

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND ISC DISCLAIMS ALL WARRANTIES WITH REGARD
   TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
   FITNESS. IN NO EVENT SHALL ISC BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT,
   OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
   USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
   TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
   OF THIS SOFTWARE.

   -----------------------------------------------------------------------------

   To release a closed-source product which uses other parts of JUCE not
   licensed under the ISC terms, commercial licenses are available: visit
   www.juce.com for more information.

  ==============================================================================
*/

namespace std
{
    /**
        This class provides an alternative to std::function that is compatible
        with OS X 10.6 and earlier. This will only be used in OS X versions 10.6
        and earlier and the Projucer live build.
    */

    template <typename>
    class function;

    template <typename Result, typename... Arguments>
    class function<Result (Arguments...)>
    {
    public:
        /** Creates an empty function. */
        function() noexcept {}

        /** Creates an empty function. */
        function (decltype (nullptr)) noexcept {}

        /** Creates a function targetting the provided Functor. */
        template <typename Functor>
        function (Functor f)
        {
            functorHolderHelper = getFunctorStorage (sizeof (FunctorHolder<Functor, Result, Arguments...>));
            new (functorHolderHelper) FunctorHolder<Functor, Result, Arguments...> (f);
        }

        /** Copy constructor. */
        function (function const& other)
        {
            copy (other);
        }

        /** Move constructor */
        function (function&& other)
        {
            move (other);
        }

        /** Destructor. */
        ~function()
        {
            release();
        }

        /** Replaces the contents of this function with the contents of another. */
        function& operator= (function const& other)
        {
            release();
            copy (other);

            return *this;
        }

        /** Moves the contents of another function into this one. */
        function& operator= (function&& other)
        {
            release();
            move (other);

            return *this;
        }

        /** Allows conditional expressions to test if this function is empty. */
        explicit operator bool() const noexcept
        {
            return functorHolderHelper != nullptr;
        }

        /** Swaps the contents of this function with another. After this operation the
            two functions will be pointing at each other's targets. */
        void swap (function& other)
        {
            function<Result (Arguments...)> tmp (*this);
            *this = other;
            other = tmp;
        }

        /** Invokes the target of this function. */
        Result operator() (Arguments... args) const
        {
            return (*functorHolderHelper) (args...);
        }

        bool operator== (decltype (nullptr)) const noexcept     { return (functorHolderHelper == nullptr); }
        bool operator!= (decltype (nullptr)) const noexcept     { return (functorHolderHelper != nullptr); }

    private:
        //==============================================================================
        template <typename ReturnType, typename... Args>
        struct FunctorHolderBase
        {
            virtual ~FunctorHolderBase() {}
            virtual int getSize() const noexcept = 0;
            virtual void copy (void*) const = 0;
            virtual ReturnType operator()(Args...) = 0;
        };

        template <typename Functor, typename ReturnType, typename... Args>
        struct FunctorHolder : FunctorHolderBase<Result, Arguments...>
        {
            FunctorHolder (Functor func) : f (func) {}

            int getSize() const noexcept override final
            {
                return sizeof (*this);
            }

            void copy (void* destination) const override final
            {
                new (destination) FunctorHolder (f);
            }

            ReturnType operator()(Args... args) override final
            {
                return f (args...);
            }

            Functor f;
        };

        FunctorHolderBase<Result, Arguments...>* getFunctorStorage (int size)
        {
            return reinterpret_cast<FunctorHolderBase<Result, Arguments...>*>
                       (size > functorHolderStackSize ? new char [size]
                                                      : &(stackFunctorStorage[0]));
        }

        void copy (function const& other)
        {
            if (other.functorHolderHelper != nullptr)
            {
                functorHolderHelper = getFunctorStorage (other.functorHolderHelper->getSize());
                other.functorHolderHelper->copy (functorHolderHelper);
            }
        }

        void move (function& other)
        {
            if (other.functorHolderHelper != nullptr)
            {
                if (other.functorHolderHelper->getSize() > functorHolderStackSize)
                {
                    functorHolderHelper = other.functorHolderHelper;
                }
                else
                {
                    std::copy (other.stackFunctorStorage, other.stackFunctorStorage + functorHolderStackSize,
                               stackFunctorStorage);
                    functorHolderHelper = reinterpret_cast<FunctorHolderBase<Result, Arguments...>*> (&(stackFunctorStorage[0]));
                }

                other.functorHolderHelper = nullptr;
            }
        }

        void release()
        {
            if (functorHolderHelper != nullptr)
            {
                if (functorHolderHelper->getSize() > functorHolderStackSize)
                    delete[] reinterpret_cast<char*> (functorHolderHelper);
                else
                    functorHolderHelper->~FunctorHolderBase<Result, Arguments...>();

                functorHolderHelper = nullptr;
            }
        }

        static const int functorHolderStackSize = 24;
        char stackFunctorStorage[functorHolderStackSize];

        FunctorHolderBase<Result, Arguments...>* functorHolderHelper = nullptr;
    };
}
