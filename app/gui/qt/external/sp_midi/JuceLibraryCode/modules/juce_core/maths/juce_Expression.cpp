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

class Expression::Term  : public SingleThreadedReferenceCountedObject
{
public:
    Term() {}
    virtual ~Term() {}

    virtual Type getType() const noexcept = 0;
    virtual Term* clone() const = 0;
    virtual ReferenceCountedObjectPtr<Term> resolve (const Scope&, int recursionDepth) = 0;
    virtual String toString() const = 0;
    virtual double toDouble() const                                          { return 0; }
    virtual int getInputIndexFor (const Term*) const                         { return -1; }
    virtual int getOperatorPrecedence() const                                { return 0; }
    virtual int getNumInputs() const                                         { return 0; }
    virtual Term* getInput (int) const                                       { return nullptr; }
    virtual ReferenceCountedObjectPtr<Term> negated();

    virtual ReferenceCountedObjectPtr<Term> createTermToEvaluateInput (const Scope&, const Term* /*inputTerm*/,
                                                                       double /*overallTarget*/, Term* /*topLevelTerm*/) const
    {
        jassertfalse;
        return ReferenceCountedObjectPtr<Term>();
    }

    virtual String getName() const
    {
        jassertfalse; // You shouldn't call this for an expression that's not actually a function!
        return {};
    }

    virtual void renameSymbol (const Symbol& oldSymbol, const String& newName, const Scope& scope, int recursionDepth)
    {
        for (int i = getNumInputs(); --i >= 0;)
            getInput (i)->renameSymbol (oldSymbol, newName, scope, recursionDepth);
    }

    class SymbolVisitor
    {
    public:
        virtual ~SymbolVisitor() {}
        virtual void useSymbol (const Symbol&) = 0;
    };

    virtual void visitAllSymbols (SymbolVisitor& visitor, const Scope& scope, int recursionDepth)
    {
        for (int i = getNumInputs(); --i >= 0;)
            getInput(i)->visitAllSymbols (visitor, scope, recursionDepth);
    }

private:
    JUCE_DECLARE_NON_COPYABLE (Term)
};


//==============================================================================
struct Expression::Helpers
{
    using TermPtr = ReferenceCountedObjectPtr<Term>;

    static void checkRecursionDepth (int depth)
    {
        if (depth > 256)
            throw EvaluationError ("Recursive symbol references");
    }

    friend class Expression::Term;

    //==============================================================================
    /** An exception that can be thrown by Expression::evaluate(). */
    class EvaluationError  : public std::exception
    {
    public:
        EvaluationError (const String& desc)  : description (desc)
        {
            DBG ("Expression::EvaluationError: " + description);
        }

        String description;
    };

    //==============================================================================
    class Constant  : public Term
    {
    public:
        Constant (double val, bool resolutionTarget)
            : value (val), isResolutionTarget (resolutionTarget) {}

        Type getType() const noexcept                { return constantType; }
        Term* clone() const                          { return new Constant (value, isResolutionTarget); }
        TermPtr resolve (const Scope&, int)          { return *this; }
        double toDouble() const                      { return value; }
        TermPtr negated()                            { return *new Constant (-value, isResolutionTarget); }

        String toString() const
        {
            String s (value);
            if (isResolutionTarget)
                s = "@" + s;

            return s;
        }

        double value;
        bool isResolutionTarget;
    };

    //==============================================================================
    class BinaryTerm  : public Term
    {
    public:
        BinaryTerm (TermPtr l, TermPtr r) : left (std::move (l)), right (std::move (r))
        {
            jassert (left != nullptr && right != nullptr);
        }

        int getInputIndexFor (const Term* possibleInput) const
        {
            return possibleInput == left ? 0 : (possibleInput == right ? 1 : -1);
        }

        Type getType() const noexcept       { return operatorType; }
        int getNumInputs() const            { return 2; }
        Term* getInput (int index) const    { return index == 0 ? left.get() : (index == 1 ? right.get() : nullptr); }

        virtual double performFunction (double left, double right) const = 0;
        virtual void writeOperator (String& dest) const = 0;

        TermPtr resolve (const Scope& scope, int recursionDepth)
        {
            return *new Constant (performFunction (left ->resolve (scope, recursionDepth)->toDouble(),
                                                   right->resolve (scope, recursionDepth)->toDouble()), false);
        }

        String toString() const
        {
            String s;
            auto ourPrecendence = getOperatorPrecedence();

            if (left->getOperatorPrecedence() > ourPrecendence)
                s << '(' << left->toString() << ')';
            else
                s = left->toString();

            writeOperator (s);

            if (right->getOperatorPrecedence() >= ourPrecendence)
                s << '(' << right->toString() << ')';
            else
                s << right->toString();

            return s;
        }

    protected:
        const TermPtr left, right;

        TermPtr createDestinationTerm (const Scope& scope, const Term* input, double overallTarget, Term* topLevelTerm) const
        {
            jassert (input == left || input == right);
            if (input != left && input != right)
                return {};

            if (auto dest = findDestinationFor (topLevelTerm, this))
                return dest->createTermToEvaluateInput (scope, this, overallTarget, topLevelTerm);

            return *new Constant (overallTarget, false);
        }
    };

    //==============================================================================
    class SymbolTerm  : public Term
    {
    public:
        explicit SymbolTerm (const String& sym) : symbol (sym) {}

        TermPtr resolve (const Scope& scope, int recursionDepth)
        {
            checkRecursionDepth (recursionDepth);
            return scope.getSymbolValue (symbol).term->resolve (scope, recursionDepth + 1);
        }

        Type getType() const noexcept   { return symbolType; }
        Term* clone() const             { return new SymbolTerm (symbol); }
        String toString() const         { return symbol; }
        String getName() const          { return symbol; }

        void visitAllSymbols (SymbolVisitor& visitor, const Scope& scope, int recursionDepth)
        {
            checkRecursionDepth (recursionDepth);
            visitor.useSymbol (Symbol (scope.getScopeUID(), symbol));
            scope.getSymbolValue (symbol).term->visitAllSymbols (visitor, scope, recursionDepth + 1);
        }

        void renameSymbol (const Symbol& oldSymbol, const String& newName, const Scope& scope, int /*recursionDepth*/)
        {
            if (oldSymbol.symbolName == symbol && scope.getScopeUID() == oldSymbol.scopeUID)
                symbol = newName;
        }

        String symbol;
    };

    //==============================================================================
    class Function  : public Term
    {
    public:
        explicit Function (const String& name)  : functionName (name) {}

        Function (const String& name, const Array<Expression>& params)
            : functionName (name), parameters (params)
        {}

        Type getType() const noexcept   { return functionType; }
        Term* clone() const             { return new Function (functionName, parameters); }
        int getNumInputs() const        { return parameters.size(); }
        Term* getInput (int i) const    { return parameters.getReference(i).term.get(); }
        String getName() const          { return functionName; }

        TermPtr resolve (const Scope& scope, int recursionDepth)
        {
            checkRecursionDepth (recursionDepth);
            double result = 0;
            auto numParams = parameters.size();

            if (numParams > 0)
            {
                HeapBlock<double> params (numParams);

                for (int i = 0; i < numParams; ++i)
                    params[i] = parameters.getReference(i).term->resolve (scope, recursionDepth + 1)->toDouble();

                result = scope.evaluateFunction (functionName, params, numParams);
            }
            else
            {
                result = scope.evaluateFunction (functionName, nullptr, 0);
            }

            return *new Constant (result, false);
        }

        int getInputIndexFor (const Term* possibleInput) const
        {
            for (int i = 0; i < parameters.size(); ++i)
                if (parameters.getReference(i).term == possibleInput)
                    return i;

            return -1;
        }

        String toString() const
        {
            if (parameters.size() == 0)
                return functionName + "()";

            String s (functionName + " (");

            for (int i = 0; i < parameters.size(); ++i)
            {
                s << parameters.getReference(i).term->toString();

                if (i < parameters.size() - 1)
                    s << ", ";
            }

            s << ')';
            return s;
        }

        const String functionName;
        Array<Expression> parameters;
    };

    //==============================================================================
    class DotOperator  : public BinaryTerm
    {
    public:
        DotOperator (SymbolTerm* l, TermPtr r)  : BinaryTerm (TermPtr (l), r) {}

        TermPtr resolve (const Scope& scope, int recursionDepth)
        {
            checkRecursionDepth (recursionDepth);

            EvaluationVisitor visitor (right, recursionDepth + 1);
            scope.visitRelativeScope (getSymbol()->symbol, visitor);
            return visitor.output;
        }

        Term* clone() const                             { return new DotOperator (getSymbol(), *right); }
        String getName() const                          { return "."; }
        int getOperatorPrecedence() const               { return 1; }
        void writeOperator (String& dest) const         { dest << '.'; }
        double performFunction (double, double) const   { return 0.0; }

        void visitAllSymbols (SymbolVisitor& visitor, const Scope& scope, int recursionDepth)
        {
            checkRecursionDepth (recursionDepth);
            visitor.useSymbol (Symbol (scope.getScopeUID(), getSymbol()->symbol));

            SymbolVisitingVisitor v (right, visitor, recursionDepth + 1);

            try
            {
                scope.visitRelativeScope (getSymbol()->symbol, v);
            }
            catch (...) {}
        }

        void renameSymbol (const Symbol& oldSymbol, const String& newName, const Scope& scope, int recursionDepth)
        {
            checkRecursionDepth (recursionDepth);
            getSymbol()->renameSymbol (oldSymbol, newName, scope, recursionDepth);

            SymbolRenamingVisitor visitor (right, oldSymbol, newName, recursionDepth + 1);

            try
            {
                scope.visitRelativeScope (getSymbol()->symbol, visitor);
            }
            catch (...) {}
        }

    private:
        //==============================================================================
        class EvaluationVisitor  : public Scope::Visitor
        {
        public:
            EvaluationVisitor (const TermPtr& t, const int recursion)
                : input (t), output (t), recursionCount (recursion) {}

            void visit (const Scope& scope)   { output = input->resolve (scope, recursionCount); }

            const TermPtr input;
            TermPtr output;
            const int recursionCount;

        private:
            JUCE_DECLARE_NON_COPYABLE (EvaluationVisitor)
        };

        class SymbolVisitingVisitor  : public Scope::Visitor
        {
        public:
            SymbolVisitingVisitor (const TermPtr& t, SymbolVisitor& v, const int recursion)
                : input (t), visitor (v), recursionCount (recursion) {}

            void visit (const Scope& scope)   { input->visitAllSymbols (visitor, scope, recursionCount); }

        private:
            const TermPtr input;
            SymbolVisitor& visitor;
            const int recursionCount;

            JUCE_DECLARE_NON_COPYABLE (SymbolVisitingVisitor)
        };

        class SymbolRenamingVisitor   : public Scope::Visitor
        {
        public:
            SymbolRenamingVisitor (const TermPtr& t, const Expression::Symbol& symbol_, const String& newName_, const int recursionCount_)
                : input (t), symbol (symbol_), newName (newName_), recursionCount (recursionCount_)  {}

            void visit (const Scope& scope)   { input->renameSymbol (symbol, newName, scope, recursionCount); }

        private:
            const TermPtr input;
            const Symbol& symbol;
            const String newName;
            const int recursionCount;

            JUCE_DECLARE_NON_COPYABLE (SymbolRenamingVisitor)
        };

        SymbolTerm* getSymbol() const  { return static_cast<SymbolTerm*> (left.get()); }

        JUCE_DECLARE_NON_COPYABLE (DotOperator)
    };

    //==============================================================================
    class Negate  : public Term
    {
    public:
        explicit Negate (const TermPtr& t) : input (t)
        {
            jassert (t != nullptr);
        }

        Type getType() const noexcept                           { return operatorType; }
        int getInputIndexFor (const Term* possibleInput) const  { return possibleInput == input ? 0 : -1; }
        int getNumInputs() const                                { return 1; }
        Term* getInput (int index) const                        { return index == 0 ? input.get() : nullptr; }
        Term* clone() const                                     { return new Negate (*input->clone()); }

        TermPtr resolve (const Scope& scope, int recursionDepth)
        {
            return *new Constant (-input->resolve (scope, recursionDepth)->toDouble(), false);
        }

        String getName() const          { return "-"; }
        TermPtr negated()               { return input; }

        TermPtr createTermToEvaluateInput (const Scope& scope, const Term* t, double overallTarget, Term* topLevelTerm) const
        {
            ignoreUnused (t);
            jassert (t == input);

            const Term* const dest = findDestinationFor (topLevelTerm, this);

            return *new Negate (dest == nullptr ? TermPtr (*new Constant (overallTarget, false))
                                                : dest->createTermToEvaluateInput (scope, this, overallTarget, topLevelTerm));
        }

        String toString() const
        {
            if (input->getOperatorPrecedence() > 0)
                return "-(" + input->toString() + ")";

            return "-" + input->toString();
        }

    private:
        const TermPtr input;
    };

    //==============================================================================
    class Add  : public BinaryTerm
    {
    public:
        Add (TermPtr l, TermPtr r) : BinaryTerm (l, r) {}

        Term* clone() const                     { return new Add (*left->clone(), *right->clone()); }
        double performFunction (double lhs, double rhs) const    { return lhs + rhs; }
        int getOperatorPrecedence() const       { return 3; }
        String getName() const                  { return "+"; }
        void writeOperator (String& dest) const { dest << " + "; }

        TermPtr createTermToEvaluateInput (const Scope& scope, const Term* input, double overallTarget, Term* topLevelTerm) const
        {
            if (auto newDest = createDestinationTerm (scope, input, overallTarget, topLevelTerm))
                return *new Subtract (newDest, *(input == left ? right : left)->clone());

            return {};
        }

    private:
        JUCE_DECLARE_NON_COPYABLE (Add)
    };

    //==============================================================================
    class Subtract  : public BinaryTerm
    {
    public:
        Subtract (TermPtr l, TermPtr r) : BinaryTerm (l, r) {}

        Term* clone() const                     { return new Subtract (*left->clone(), *right->clone()); }
        double performFunction (double lhs, double rhs) const    { return lhs - rhs; }
        int getOperatorPrecedence() const       { return 3; }
        String getName() const                  { return "-"; }
        void writeOperator (String& dest) const { dest << " - "; }

        TermPtr createTermToEvaluateInput (const Scope& scope, const Term* input, double overallTarget, Term* topLevelTerm) const
        {
            if (auto newDest = createDestinationTerm (scope, input, overallTarget, topLevelTerm))
            {
                if (input == left)
                    return *new Add (*newDest, *right->clone());

                return *new Subtract (*left->clone(), *newDest);
            }

            return {};
        }

    private:
        JUCE_DECLARE_NON_COPYABLE (Subtract)
    };

    //==============================================================================
    class Multiply  : public BinaryTerm
    {
    public:
        Multiply (TermPtr l, TermPtr r) : BinaryTerm (l, r) {}

        Term* clone() const                     { return new Multiply (*left->clone(), *right->clone()); }
        double performFunction (double lhs, double rhs) const    { return lhs * rhs; }
        String getName() const                  { return "*"; }
        void writeOperator (String& dest) const { dest << " * "; }
        int getOperatorPrecedence() const       { return 2; }

        TermPtr createTermToEvaluateInput (const Scope& scope, const Term* input, double overallTarget, Term* topLevelTerm) const
        {
            if (auto newDest = createDestinationTerm (scope, input, overallTarget, topLevelTerm))
                return *new Divide (newDest, *(input == left ? right : left)->clone());

            return {};
        }

        JUCE_DECLARE_NON_COPYABLE (Multiply)
    };

    //==============================================================================
    class Divide  : public BinaryTerm
    {
    public:
        Divide (TermPtr l, TermPtr r) : BinaryTerm (l, r) {}

        Term* clone() const                     { return new Divide (*left->clone(), *right->clone()); }
        double performFunction (double lhs, double rhs) const    { return lhs / rhs; }
        String getName() const                  { return "/"; }
        void writeOperator (String& dest) const { dest << " / "; }
        int getOperatorPrecedence() const       { return 2; }

        TermPtr createTermToEvaluateInput (const Scope& scope, const Term* input, double overallTarget, Term* topLevelTerm) const
        {
            auto newDest = createDestinationTerm (scope, input, overallTarget, topLevelTerm);

            if (newDest == nullptr)
                return {};

            if (input == left)
                return *new Multiply (*newDest, *right->clone());

            return *new Divide (*left->clone(), *newDest);
        }

        JUCE_DECLARE_NON_COPYABLE (Divide)
    };

    //==============================================================================
    static Term* findDestinationFor (Term* const topLevel, const Term* const inputTerm)
    {
        const int inputIndex = topLevel->getInputIndexFor (inputTerm);
        if (inputIndex >= 0)
            return topLevel;

        for (int i = topLevel->getNumInputs(); --i >= 0;)
        {
            Term* const t = findDestinationFor (topLevel->getInput (i), inputTerm);

            if (t != nullptr)
                return t;
        }

        return nullptr;
    }

    static Constant* findTermToAdjust (Term* const term, const bool mustBeFlagged)
    {
        jassert (term != nullptr);

        if (term->getType() == constantType)
        {
            Constant* const c = static_cast<Constant*> (term);
            if (c->isResolutionTarget || ! mustBeFlagged)
                return c;
        }

        if (term->getType() == functionType)
            return nullptr;

        const int numIns = term->getNumInputs();

        for (int i = 0; i < numIns; ++i)
        {
            Term* const input = term->getInput (i);

            if (input->getType() == constantType)
            {
                Constant* const c = static_cast<Constant*> (input);

                if (c->isResolutionTarget || ! mustBeFlagged)
                    return c;
            }
        }

        for (int i = 0; i < numIns; ++i)
            if (auto c = findTermToAdjust (term->getInput (i), mustBeFlagged))
                return c;

        return nullptr;
    }

    static bool containsAnySymbols (const Term& t)
    {
        if (t.getType() == Expression::symbolType)
            return true;

        for (int i = t.getNumInputs(); --i >= 0;)
            if (containsAnySymbols (*t.getInput (i)))
                return true;

        return false;
    }

    //==============================================================================
    class SymbolCheckVisitor  : public Term::SymbolVisitor
    {
    public:
        SymbolCheckVisitor (const Symbol& s) : symbol (s) {}
        void useSymbol (const Symbol& s)    { wasFound = wasFound || s == symbol; }

        bool wasFound = false;

    private:
        const Symbol& symbol;

        JUCE_DECLARE_NON_COPYABLE (SymbolCheckVisitor)
    };

    //==============================================================================
    class SymbolListVisitor  : public Term::SymbolVisitor
    {
    public:
        SymbolListVisitor (Array<Symbol>& list_) : list (list_) {}
        void useSymbol (const Symbol& s)    { list.addIfNotAlreadyThere (s); }

    private:
        Array<Symbol>& list;

        JUCE_DECLARE_NON_COPYABLE (SymbolListVisitor)
    };

    //==============================================================================
    class Parser
    {
    public:
        //==============================================================================
        Parser (String::CharPointerType& stringToParse)  : text (stringToParse)
        {
        }

        TermPtr readUpToComma()
        {
            if (text.isEmpty())
                return *new Constant (0.0, false);

            auto e = readExpression();

            if (e == nullptr || ((! readOperator (",")) && ! text.isEmpty()))
                return parseError ("Syntax error: \"" + String (text) + "\"");

            return e;
        }

        String error;

    private:
        String::CharPointerType& text;

        TermPtr parseError (const String& message)
        {
            if (error.isEmpty())
                error = message;

            return {};
        }

        //==============================================================================
        static inline bool isDecimalDigit (const juce_wchar c) noexcept
        {
            return c >= '0' && c <= '9';
        }

        bool readChar (const juce_wchar required) noexcept
        {
            if (*text == required)
            {
                ++text;
                return true;
            }

            return false;
        }

        bool readOperator (const char* ops, char* const opType = nullptr) noexcept
        {
            text = text.findEndOfWhitespace();

            while (*ops != 0)
            {
                if (readChar ((juce_wchar) (uint8) *ops))
                {
                    if (opType != nullptr)
                        *opType = *ops;

                    return true;
                }

                ++ops;
            }

            return false;
        }

        bool readIdentifier (String& identifier) noexcept
        {
            text = text.findEndOfWhitespace();
            auto t = text;
            int numChars = 0;

            if (t.isLetter() || *t == '_')
            {
                ++t;
                ++numChars;

                while (t.isLetterOrDigit() || *t == '_')
                {
                    ++t;
                    ++numChars;
                }
            }

            if (numChars > 0)
            {
                identifier = String (text, (size_t) numChars);
                text = t;
                return true;
            }

            return false;
        }

        Term* readNumber() noexcept
        {
            text = text.findEndOfWhitespace();
            auto t = text;
            bool isResolutionTarget = (*t == '@');

            if (isResolutionTarget)
            {
                ++t;
                t = t.findEndOfWhitespace();
                text = t;
            }

            if (*t == '-')
            {
                ++t;
                t = t.findEndOfWhitespace();
            }

            if (isDecimalDigit (*t) || (*t == '.' && isDecimalDigit (t[1])))
                return new Constant (CharacterFunctions::readDoubleValue (text), isResolutionTarget);

            return nullptr;
        }

        TermPtr readExpression()
        {
            auto lhs = readMultiplyOrDivideExpression();
            char opType;

            while (lhs != nullptr && readOperator ("+-", &opType))
            {
                auto rhs = readMultiplyOrDivideExpression();

                if (rhs == nullptr)
                    return parseError ("Expected expression after \"" + String::charToString ((juce_wchar) (uint8) opType) + "\"");

                if (opType == '+')
                    lhs = *new Add (lhs, rhs);
                else
                    lhs = *new Subtract (lhs, rhs);
            }

            return lhs;
        }

        TermPtr readMultiplyOrDivideExpression()
        {
            auto lhs = readUnaryExpression();
            char opType;

            while (lhs != nullptr && readOperator ("*/", &opType))
            {
                TermPtr rhs (readUnaryExpression());

                if (rhs == nullptr)
                    return parseError ("Expected expression after \"" + String::charToString ((juce_wchar) (uint8) opType) + "\"");

                if (opType == '*')
                    lhs = *new Multiply (lhs, rhs);
                else
                    lhs = *new Divide (lhs, rhs);
            }

            return lhs;
        }

        TermPtr readUnaryExpression()
        {
            char opType;
            if (readOperator ("+-", &opType))
            {
                TermPtr e (readUnaryExpression());

                if (e == nullptr)
                    return parseError ("Expected expression after \"" + String::charToString ((juce_wchar) (uint8) opType) + "\"");

                if (opType == '-')
                    e = e->negated();

                return e;
            }

            return readPrimaryExpression();
        }

        TermPtr readPrimaryExpression()
        {
            if (auto e = readParenthesisedExpression())
                return e;

            if (auto e = readNumber())
                return e;

            return readSymbolOrFunction();
        }

        TermPtr readSymbolOrFunction()
        {
            String identifier;

            if (readIdentifier (identifier))
            {
                if (readOperator ("(")) // method call...
                {
                    auto f = new Function (identifier);
                    std::unique_ptr<Term> func (f);  // (can't use std::unique_ptr<Function> in MSVC)

                    auto param = readExpression();

                    if (param == nullptr)
                    {
                        if (readOperator (")"))
                            return TermPtr (func.release());

                        return parseError ("Expected parameters after \"" + identifier + " (\"");
                    }

                    f->parameters.add (Expression (param.get()));

                    while (readOperator (","))
                    {
                        param = readExpression();

                        if (param == nullptr)
                            return parseError ("Expected expression after \",\"");

                        f->parameters.add (Expression (param.get()));
                    }

                    if (readOperator (")"))
                        return TermPtr (func.release());

                    return parseError ("Expected \")\"");
                }

                if (readOperator ("."))
                {
                    TermPtr rhs (readSymbolOrFunction());

                    if (rhs == nullptr)
                        return parseError ("Expected symbol or function after \".\"");

                    if (identifier == "this")
                        return rhs;

                    return *new DotOperator (new SymbolTerm (identifier), rhs);
                }

                // just a symbol..
                jassert (identifier.trim() == identifier);
                return *new SymbolTerm (identifier);
            }

            return {};
        }

        TermPtr readParenthesisedExpression()
        {
            if (! readOperator ("("))
                return {};

            auto e = readExpression();

            if (e == nullptr || ! readOperator (")"))
                return {};

            return e;
        }

        JUCE_DECLARE_NON_COPYABLE (Parser)
    };
};

//==============================================================================
Expression::Expression()
    : term (new Expression::Helpers::Constant (0, false))
{
}

Expression::~Expression()
{
}

Expression::Expression (Term* t) : term (t)
{
    jassert (term != nullptr);
}

Expression::Expression (const double constant)
    : term (new Expression::Helpers::Constant (constant, false))
{
}

Expression::Expression (const Expression& other)
    : term (other.term)
{
}

Expression& Expression::operator= (const Expression& other)
{
    term = other.term;
    return *this;
}

Expression::Expression (Expression&& other) noexcept
    : term (std::move (other.term))
{
}

Expression& Expression::operator= (Expression&& other) noexcept
{
    term = std::move (other.term);
    return *this;
}

Expression::Expression (const String& stringToParse, String& parseError)
{
    auto text = stringToParse.getCharPointer();
    Helpers::Parser parser (text);
    term = parser.readUpToComma();
    parseError = parser.error;
}

Expression Expression::parse (String::CharPointerType& stringToParse, String& parseError)
{
    Helpers::Parser parser (stringToParse);
    Expression e (parser.readUpToComma().get());
    parseError = parser.error;
    return e;
}

double Expression::evaluate() const
{
    return evaluate (Expression::Scope());
}

double Expression::evaluate (const Expression::Scope& scope) const
{
    String err;
    return evaluate (scope, err);
}

double Expression::evaluate (const Scope& scope, String& evaluationError) const
{
    try
    {
        return term->resolve (scope, 0)->toDouble();
    }
    catch (Helpers::EvaluationError& e)
    {
        evaluationError = e.description;
    }

    return 0;
}

Expression Expression::operator+ (const Expression& other) const  { return Expression (new Helpers::Add (term, other.term)); }
Expression Expression::operator- (const Expression& other) const  { return Expression (new Helpers::Subtract (term, other.term)); }
Expression Expression::operator* (const Expression& other) const  { return Expression (new Helpers::Multiply (term, other.term)); }
Expression Expression::operator/ (const Expression& other) const  { return Expression (new Helpers::Divide (term, other.term)); }
Expression Expression::operator-() const                          { return Expression (term->negated().get()); }
Expression Expression::symbol (const String& symbol)              { return Expression (new Helpers::SymbolTerm (symbol)); }

Expression Expression::function (const String& functionName, const Array<Expression>& parameters)
{
    return Expression (new Helpers::Function (functionName, parameters));
}

Expression Expression::adjustedToGiveNewResult (const double targetValue, const Expression::Scope& scope) const
{
    std::unique_ptr<Term> newTerm (term->clone());

    auto termToAdjust = Helpers::findTermToAdjust (newTerm.get(), true);

    if (termToAdjust == nullptr)
        termToAdjust = Helpers::findTermToAdjust (newTerm.get(), false);

    if (termToAdjust == nullptr)
    {
        newTerm.reset (new Helpers::Add (*newTerm.release(), *new Helpers::Constant (0, false)));
        termToAdjust = Helpers::findTermToAdjust (newTerm.get(), false);
    }

    jassert (termToAdjust != nullptr);

    if (const Term* parent = Helpers::findDestinationFor (newTerm.get(), termToAdjust))
    {
        if (Helpers::TermPtr reverseTerm = parent->createTermToEvaluateInput (scope, termToAdjust, targetValue, newTerm.get()))
            termToAdjust->value = Expression (reverseTerm.get()).evaluate (scope);
        else
            return Expression (targetValue);
    }
    else
    {
        termToAdjust->value = targetValue;
    }

    return Expression (newTerm.release());
}

Expression Expression::withRenamedSymbol (const Expression::Symbol& oldSymbol, const String& newName, const Scope& scope) const
{
    jassert (newName.toLowerCase().containsOnly ("abcdefghijklmnopqrstuvwxyz0123456789_"));

    if (oldSymbol.symbolName == newName)
        return *this;

    Expression e (term->clone());
    e.term->renameSymbol (oldSymbol, newName, scope, 0);
    return e;
}

bool Expression::referencesSymbol (const Expression::Symbol& symbolToCheck, const Scope& scope) const
{
    Helpers::SymbolCheckVisitor visitor (symbolToCheck);

    try
    {
        term->visitAllSymbols (visitor, scope, 0);
    }
    catch (Helpers::EvaluationError&)
    {}

    return visitor.wasFound;
}

void Expression::findReferencedSymbols (Array<Symbol>& results, const Scope& scope) const
{
    try
    {
        Helpers::SymbolListVisitor visitor (results);
        term->visitAllSymbols (visitor, scope, 0);
    }
    catch (Helpers::EvaluationError&)
    {}
}

String Expression::toString() const                     { return term->toString(); }
bool Expression::usesAnySymbols() const                 { return Helpers::containsAnySymbols (*term); }
Expression::Type Expression::getType() const noexcept   { return term->getType(); }
String Expression::getSymbolOrFunction() const          { return term->getName(); }
int Expression::getNumInputs() const                    { return term->getNumInputs(); }
Expression Expression::getInput (int index) const       { return Expression (term->getInput (index)); }

//==============================================================================
ReferenceCountedObjectPtr<Expression::Term> Expression::Term::negated()
{
    return *new Helpers::Negate (*this);
}

//==============================================================================
Expression::Symbol::Symbol (const String& scope, const String& symbol)
    : scopeUID (scope), symbolName (symbol)
{
}

bool Expression::Symbol::operator== (const Symbol& other) const noexcept
{
    return symbolName == other.symbolName && scopeUID == other.scopeUID;
}

bool Expression::Symbol::operator!= (const Symbol& other) const noexcept
{
    return ! operator== (other);
}

//==============================================================================
Expression::Scope::Scope()  {}
Expression::Scope::~Scope() {}

Expression Expression::Scope::getSymbolValue (const String& symbol) const
{
    if (symbol.isNotEmpty())
        throw Helpers::EvaluationError ("Unknown symbol: " + symbol);

    return Expression();
}

double Expression::Scope::evaluateFunction (const String& functionName, const double* parameters, int numParams) const
{
    if (numParams > 0)
    {
        if (functionName == "min")
        {
            double v = parameters[0];
            for (int i = 1; i < numParams; ++i)
                v = jmin (v, parameters[i]);

            return v;
        }

        if (functionName == "max")
        {
            double v = parameters[0];
            for (int i = 1; i < numParams; ++i)
                v = jmax (v, parameters[i]);

            return v;
        }

        if (numParams == 1)
        {
            if (functionName == "sin")  return std::sin (parameters[0]);
            if (functionName == "cos")  return std::cos (parameters[0]);
            if (functionName == "tan")  return std::tan (parameters[0]);
            if (functionName == "abs")  return std::abs (parameters[0]);
        }
    }

    throw Helpers::EvaluationError ("Unknown function: \"" + functionName + "\"");
}

void Expression::Scope::visitRelativeScope (const String& scopeName, Visitor&) const
{
    throw Helpers::EvaluationError ("Unknown symbol: " + scopeName);
}

String Expression::Scope::getScopeUID() const
{
    return {};
}

} // namespace juce
