/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2020 - Raw Material Software Limited

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

#define JUCE_JS_OPERATORS(X) \
    X(semicolon,     ";")        X(dot,          ".")       X(comma,        ",") \
    X(openParen,     "(")        X(closeParen,   ")")       X(openBrace,    "{")    X(closeBrace, "}") \
    X(openBracket,   "[")        X(closeBracket, "]")       X(colon,        ":")    X(question,   "?") \
    X(typeEquals,    "===")      X(equals,       "==")      X(assign,       "=") \
    X(typeNotEquals, "!==")      X(notEquals,    "!=")      X(logicalNot,   "!") \
    X(plusEquals,    "+=")       X(plusplus,     "++")      X(plus,         "+") \
    X(minusEquals,   "-=")       X(minusminus,   "--")      X(minus,        "-") \
    X(timesEquals,   "*=")       X(times,        "*")       X(divideEquals, "/=")   X(divide,     "/") \
    X(moduloEquals,  "%=")       X(modulo,       "%")       X(xorEquals,    "^=")   X(bitwiseXor, "^") \
    X(andEquals,     "&=")       X(logicalAnd,   "&&")      X(bitwiseAnd,   "&") \
    X(orEquals,      "|=")       X(logicalOr,    "||")      X(bitwiseOr,    "|") \
    X(leftShiftEquals,    "<<=") X(lessThanOrEqual,  "<=")  X(leftShift,    "<<")   X(lessThan,   "<") \
    X(rightShiftUnsigned, ">>>") X(rightShiftEquals, ">>=") X(rightShift,   ">>")   X(greaterThanOrEqual, ">=")  X(greaterThan,  ">")

#define JUCE_JS_KEYWORDS(X) \
    X(var,      "var")      X(if_,     "if")     X(else_,  "else")   X(do_,       "do")       X(null_,     "null") \
    X(while_,   "while")    X(for_,    "for")    X(break_, "break")  X(continue_, "continue") X(undefined, "undefined") \
    X(function, "function") X(return_, "return") X(true_,  "true")   X(false_,    "false")    X(new_,      "new") \
    X(typeof_,  "typeof")

namespace TokenTypes
{
    #define JUCE_DECLARE_JS_TOKEN(name, str)  static const char* const name = str;
    JUCE_JS_KEYWORDS  (JUCE_DECLARE_JS_TOKEN)
    JUCE_JS_OPERATORS (JUCE_DECLARE_JS_TOKEN)
    JUCE_DECLARE_JS_TOKEN (eof,        "$eof")
    JUCE_DECLARE_JS_TOKEN (literal,    "$literal")
    JUCE_DECLARE_JS_TOKEN (identifier, "$identifier")
}

JUCE_BEGIN_IGNORE_WARNINGS_MSVC (4702)

//==============================================================================
struct JavascriptEngine::RootObject   : public DynamicObject
{
    RootObject()
    {
        setMethod ("exec",       exec);
        setMethod ("eval",       eval);
        setMethod ("trace",      trace);
        setMethod ("charToInt",  charToInt);
        setMethod ("parseInt",   IntegerClass::parseInt);
        setMethod ("typeof",     typeof_internal);
        setMethod ("parseFloat", parseFloat);
    }

    Time timeout;

    using Args = const var::NativeFunctionArgs&;
    using TokenType = const char*;

    void execute (const String& code)
    {
        ExpressionTreeBuilder tb (code);
        std::unique_ptr<BlockStatement> (tb.parseStatementList())->perform (Scope ({}, *this, *this), nullptr);
    }

    var evaluate (const String& code)
    {
        ExpressionTreeBuilder tb (code);
        return ExpPtr (tb.parseExpression())->getResult (Scope ({}, *this, *this));
    }

    //==============================================================================
    static bool areTypeEqual (const var& a, const var& b)
    {
        return a.hasSameTypeAs (b) && isFunction (a) == isFunction (b)
                && (((a.isUndefined() || a.isVoid()) && (b.isUndefined() || b.isVoid())) || a == b);
    }

    static String getTokenName (TokenType t)                  { return t[0] == '$' ? String (t + 1) : ("'" + String (t) + "'"); }
    static bool isFunction (const var& v) noexcept            { return dynamic_cast<FunctionObject*> (v.getObject()) != nullptr; }
    static bool isNumeric (const var& v) noexcept             { return v.isInt() || v.isDouble() || v.isInt64() || v.isBool(); }
    static bool isNumericOrUndefined (const var& v) noexcept  { return isNumeric (v) || v.isUndefined(); }
    static int64 getOctalValue (const String& s)              { BigInteger b; b.parseString (s.initialSectionContainingOnly ("01234567"), 8); return b.toInt64(); }
    static Identifier getPrototypeIdentifier()                { static const Identifier i ("prototype"); return i; }
    static var* getPropertyPointer (DynamicObject& o, const Identifier& i) noexcept   { return o.getProperties().getVarPointer (i); }

    //==============================================================================
    struct CodeLocation
    {
        CodeLocation (const String& code) noexcept        : program (code), location (program.getCharPointer()) {}
        CodeLocation (const CodeLocation& other) noexcept : program (other.program), location (other.location) {}

        void throwError (const String& message) const
        {
            int col = 1, line = 1;

            for (auto i = program.getCharPointer(); i < location && ! i.isEmpty(); ++i)
            {
                ++col;
                if (*i == '\n')  { col = 1; ++line; }
            }

            throw "Line " + String (line) + ", column " + String (col) + " : " + message;
        }

        String program;
        String::CharPointerType location;
    };

    //==============================================================================
    struct Scope
    {
        Scope (const Scope* p, ReferenceCountedObjectPtr<RootObject> rt, DynamicObject::Ptr scp) noexcept
            : parent (p), root (std::move (rt)),
              scope (std::move (scp)) {}

        const Scope* const parent;
        ReferenceCountedObjectPtr<RootObject> root;
        DynamicObject::Ptr scope;

        var findFunctionCall (const CodeLocation& location, const var& targetObject, const Identifier& functionName) const
        {
            if (auto* o = targetObject.getDynamicObject())
            {
                if (auto* prop = getPropertyPointer (*o, functionName))
                    return *prop;

                for (auto* p = o->getProperty (getPrototypeIdentifier()).getDynamicObject(); p != nullptr;
                     p = p->getProperty (getPrototypeIdentifier()).getDynamicObject())
                {
                    if (auto* prop = getPropertyPointer (*p, functionName))
                        return *prop;
                }

                // if there's a class with an overridden DynamicObject::hasMethod, this avoids an error
                if (o->hasMethod (functionName))
                    return {};
            }

            if (targetObject.isString())
                if (auto* m = findRootClassProperty (StringClass::getClassName(), functionName))
                    return *m;

            if (targetObject.isArray())
                if (auto* m = findRootClassProperty (ArrayClass::getClassName(), functionName))
                    return *m;

            if (auto* m = findRootClassProperty (ObjectClass::getClassName(), functionName))
                return *m;

            location.throwError ("Unknown function '" + functionName.toString() + "'");
            return {};
        }

        var* findRootClassProperty (const Identifier& className, const Identifier& propName) const
        {
            if (auto* cls = root->getProperty (className).getDynamicObject())
                return getPropertyPointer (*cls, propName);

            return nullptr;
        }

        var findSymbolInParentScopes (const Identifier& name) const
        {
            if (auto v = getPropertyPointer (*scope, name))
                return *v;

            return parent != nullptr ? parent->findSymbolInParentScopes (name)
                                     : var::undefined();
        }

        bool findAndInvokeMethod (const Identifier& function, const var::NativeFunctionArgs& args, var& result) const
        {
            auto* target = args.thisObject.getDynamicObject();

            if (target == nullptr || target == scope.get())
            {
                if (auto* m = getPropertyPointer (*scope, function))
                {
                    if (auto fo = dynamic_cast<FunctionObject*> (m->getObject()))
                    {
                        result = fo->invoke (*this, args);
                        return true;
                    }
                }
            }

            const auto& props = scope->getProperties();

            for (int i = 0; i < props.size(); ++i)
                if (auto* o = props.getValueAt (i).getDynamicObject())
                    if (Scope (this, *root, *o).findAndInvokeMethod (function, args, result))
                        return true;

            return false;
        }

        bool invokeMethod (const var& m, const var::NativeFunctionArgs& args, var& result) const
        {
            if (isFunction (m))
            {
                auto* target = args.thisObject.getDynamicObject();

                if (target == nullptr || target == scope.get())
                {
                    if (auto fo = dynamic_cast<FunctionObject*> (m.getObject()))
                    {
                        result = fo->invoke (*this, args);
                        return true;
                    }
                }
            }

            return false;
        }

        void checkTimeOut (const CodeLocation& location) const
        {
            if (Time::getCurrentTime() > root->timeout)
                location.throwError (root->timeout == Time() ? "Interrupted" : "Execution timed-out");
        }

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Scope)
    };

    //==============================================================================
    struct Statement
    {
        Statement (const CodeLocation& l) noexcept : location (l) {}
        virtual ~Statement() {}

        enum ResultCode  { ok = 0, returnWasHit, breakWasHit, continueWasHit };
        virtual ResultCode perform (const Scope&, var*) const  { return ok; }

        CodeLocation location;
        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Statement)
    };

    struct Expression  : public Statement
    {
        Expression (const CodeLocation& l) noexcept : Statement (l) {}

        virtual var getResult (const Scope&) const            { return var::undefined(); }
        virtual void assign (const Scope&, const var&) const  { location.throwError ("Cannot assign to this expression!"); }

        ResultCode perform (const Scope& s, var*) const override  { getResult (s); return ok; }
    };

    using ExpPtr = std::unique_ptr<Expression>;

    struct BlockStatement  : public Statement
    {
        BlockStatement (const CodeLocation& l) noexcept : Statement (l) {}

        ResultCode perform (const Scope& s, var* returnedValue) const override
        {
            for (auto* statement : statements)
                if (auto r = statement->perform (s, returnedValue))
                    return r;

            return ok;
        }

        OwnedArray<Statement> statements;
    };

    struct IfStatement  : public Statement
    {
        IfStatement (const CodeLocation& l) noexcept : Statement (l) {}

        ResultCode perform (const Scope& s, var* returnedValue) const override
        {
            return (condition->getResult(s) ? trueBranch : falseBranch)->perform (s, returnedValue);
        }

        ExpPtr condition;
        std::unique_ptr<Statement> trueBranch, falseBranch;
    };

    struct VarStatement  : public Statement
    {
        VarStatement (const CodeLocation& l) noexcept : Statement (l) {}

        ResultCode perform (const Scope& s, var*) const override
        {
            s.scope->setProperty (name, initialiser->getResult (s));
            return ok;
        }

        Identifier name;
        ExpPtr initialiser;
    };

    struct LoopStatement  : public Statement
    {
        LoopStatement (const CodeLocation& l, bool isDo) noexcept : Statement (l), isDoLoop (isDo) {}

        ResultCode perform (const Scope& s, var* returnedValue) const override
        {
            initialiser->perform (s, nullptr);

            while (isDoLoop || condition->getResult (s))
            {
                s.checkTimeOut (location);
                auto r = body->perform (s, returnedValue);

                if (r == returnWasHit)   return r;
                if (r == breakWasHit)    break;

                iterator->perform (s, nullptr);

                if (isDoLoop && r != continueWasHit && ! condition->getResult (s))
                    break;
            }

            return ok;
        }

        std::unique_ptr<Statement> initialiser, iterator, body;
        ExpPtr condition;
        bool isDoLoop;
    };

    struct ReturnStatement  : public Statement
    {
        ReturnStatement (const CodeLocation& l, Expression* v) noexcept : Statement (l), returnValue (v) {}

        ResultCode perform (const Scope& s, var* ret) const override
        {
            if (ret != nullptr)  *ret = returnValue->getResult (s);
            return returnWasHit;
        }

        ExpPtr returnValue;
    };

    struct BreakStatement  : public Statement
    {
        BreakStatement (const CodeLocation& l) noexcept : Statement (l) {}
        ResultCode perform (const Scope&, var*) const override  { return breakWasHit; }
    };

    struct ContinueStatement  : public Statement
    {
        ContinueStatement (const CodeLocation& l) noexcept : Statement (l) {}
        ResultCode perform (const Scope&, var*) const override  { return continueWasHit; }
    };

    struct LiteralValue  : public Expression
    {
        LiteralValue (const CodeLocation& l, const var& v) noexcept : Expression (l), value (v) {}
        var getResult (const Scope&) const override   { return value; }
        var value;
    };

    struct UnqualifiedName  : public Expression
    {
        UnqualifiedName (const CodeLocation& l, const Identifier& n) noexcept : Expression (l), name (n) {}

        var getResult (const Scope& s) const override  { return s.findSymbolInParentScopes (name); }

        void assign (const Scope& s, const var& newValue) const override
        {
            if (auto* v = getPropertyPointer (*s.scope, name))
                *v = newValue;
            else
                s.root->setProperty (name, newValue);
        }

        Identifier name;
    };

    struct DotOperator  : public Expression
    {
        DotOperator (const CodeLocation& l, ExpPtr& p, const Identifier& c) noexcept : Expression (l), parent (p.release()), child (c) {}

        var getResult (const Scope& s) const override
        {
            auto p = parent->getResult (s);
            static const Identifier lengthID ("length");

            if (child == lengthID)
            {
                if (auto* array = p.getArray())   return array->size();
                if (p.isString())                 return p.toString().length();
            }

            if (auto* o = p.getDynamicObject())
                if (auto* v = getPropertyPointer (*o, child))
                    return *v;

            return var::undefined();
        }

        void assign (const Scope& s, const var& newValue) const override
        {
            if (auto* o = parent->getResult (s).getDynamicObject())
                o->setProperty (child, newValue);
            else
                Expression::assign (s, newValue);
        }

        ExpPtr parent;
        Identifier child;
    };

    struct ArraySubscript  : public Expression
    {
        ArraySubscript (const CodeLocation& l) noexcept : Expression (l) {}

        var getResult (const Scope& s) const override
        {
            auto arrayVar = object->getResult (s); // must stay alive for the scope of this method
            auto key = index->getResult (s);

            if (const auto* array = arrayVar.getArray())
                if (key.isInt() || key.isInt64() || key.isDouble())
                    return (*array) [static_cast<int> (key)];

            if (auto* o = arrayVar.getDynamicObject())
                if (key.isString())
                    if (auto* v = getPropertyPointer (*o, Identifier (key)))
                        return *v;

            return var::undefined();
        }

        void assign (const Scope& s, const var& newValue) const override
        {
            auto arrayVar = object->getResult (s); // must stay alive for the scope of this method
            auto key = index->getResult (s);

            if (auto* array = arrayVar.getArray())
            {
                if (key.isInt() || key.isInt64() || key.isDouble())
                {
                    const int i = key;
                    while (array->size() < i)
                        array->add (var::undefined());

                    array->set (i, newValue);
                    return;
                }
            }

            if (auto* o = arrayVar.getDynamicObject())
            {
                if (key.isString())
                {
                    o->setProperty (Identifier (key), newValue);
                    return;
                }
            }

            Expression::assign (s, newValue);
        }

        ExpPtr object, index;
    };

    struct BinaryOperatorBase  : public Expression
    {
        BinaryOperatorBase (const CodeLocation& l, ExpPtr& a, ExpPtr& b, TokenType op) noexcept
            : Expression (l), lhs (a.release()), rhs (b.release()), operation (op) {}

        ExpPtr lhs, rhs;
        TokenType operation;
    };

    struct BinaryOperator  : public BinaryOperatorBase
    {
        BinaryOperator (const CodeLocation& l, ExpPtr& a, ExpPtr& b, TokenType op) noexcept
            : BinaryOperatorBase (l, a, b, op) {}

        virtual var getWithUndefinedArg() const                           { return var::undefined(); }
        virtual var getWithDoubles (double, double) const                 { return throwError ("Double"); }
        virtual var getWithInts (int64, int64) const                      { return throwError ("Integer"); }
        virtual var getWithArrayOrObject (const var& a, const var&) const { return throwError (a.isArray() ? "Array" : "Object"); }
        virtual var getWithStrings (const String&, const String&) const   { return throwError ("String"); }

        var getResult (const Scope& s) const override
        {
            var a (lhs->getResult (s)), b (rhs->getResult (s));

            if ((a.isUndefined() || a.isVoid()) && (b.isUndefined() || b.isVoid()))
                return getWithUndefinedArg();

            if (isNumericOrUndefined (a) && isNumericOrUndefined (b))
                return (a.isDouble() || b.isDouble()) ? getWithDoubles (a, b) : getWithInts (a, b);

            if (a.isArray() || a.isObject())
                return getWithArrayOrObject (a, b);

            return getWithStrings (a.toString(), b.toString());
        }

        var throwError (const char* typeName) const
            { location.throwError (getTokenName (operation) + " is not allowed on the " + typeName + " type"); return {}; }
    };

    struct EqualsOp  : public BinaryOperator
    {
        EqualsOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::equals) {}
        var getWithUndefinedArg() const override                               { return true; }
        var getWithDoubles (double a, double b) const override                 { return a == b; }
        var getWithInts (int64 a, int64 b) const override                      { return a == b; }
        var getWithStrings (const String& a, const String& b) const override   { return a == b; }
        var getWithArrayOrObject (const var& a, const var& b) const override   { return a == b; }
    };

    struct NotEqualsOp  : public BinaryOperator
    {
        NotEqualsOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::notEquals) {}
        var getWithUndefinedArg() const override                               { return false; }
        var getWithDoubles (double a, double b) const override                 { return a != b; }
        var getWithInts (int64 a, int64 b) const override                      { return a != b; }
        var getWithStrings (const String& a, const String& b) const override   { return a != b; }
        var getWithArrayOrObject (const var& a, const var& b) const override   { return a != b; }
    };

    struct LessThanOp  : public BinaryOperator
    {
        LessThanOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::lessThan) {}
        var getWithDoubles (double a, double b) const override                 { return a < b; }
        var getWithInts (int64 a, int64 b) const override                      { return a < b; }
        var getWithStrings (const String& a, const String& b) const override   { return a < b; }
    };

    struct LessThanOrEqualOp  : public BinaryOperator
    {
        LessThanOrEqualOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::lessThanOrEqual) {}
        var getWithDoubles (double a, double b) const override                 { return a <= b; }
        var getWithInts (int64 a, int64 b) const override                      { return a <= b; }
        var getWithStrings (const String& a, const String& b) const override   { return a <= b; }
    };

    struct GreaterThanOp  : public BinaryOperator
    {
        GreaterThanOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::greaterThan) {}
        var getWithDoubles (double a, double b) const override                 { return a > b; }
        var getWithInts (int64 a, int64 b) const override                      { return a > b; }
        var getWithStrings (const String& a, const String& b) const override   { return a > b; }
    };

    struct GreaterThanOrEqualOp  : public BinaryOperator
    {
        GreaterThanOrEqualOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::greaterThanOrEqual) {}
        var getWithDoubles (double a, double b) const override                 { return a >= b; }
        var getWithInts (int64 a, int64 b) const override                      { return a >= b; }
        var getWithStrings (const String& a, const String& b) const override   { return a >= b; }
    };

    struct AdditionOp  : public BinaryOperator
    {
        AdditionOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::plus) {}
        var getWithDoubles (double a, double b) const override                 { return a + b; }
        var getWithInts (int64 a, int64 b) const override                      { return a + b; }
        var getWithStrings (const String& a, const String& b) const override   { return a + b; }
    };

    struct SubtractionOp  : public BinaryOperator
    {
        SubtractionOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::minus) {}
        var getWithDoubles (double a, double b) const override { return a - b; }
        var getWithInts (int64 a, int64 b) const override      { return a - b; }
    };

    struct MultiplyOp  : public BinaryOperator
    {
        MultiplyOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::times) {}
        var getWithDoubles (double a, double b) const override { return a * b; }
        var getWithInts (int64 a, int64 b) const override      { return a * b; }
    };

    struct DivideOp  : public BinaryOperator
    {
        DivideOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::divide) {}
        var getWithDoubles (double a, double b) const override  { return b != 0 ? a / b : std::numeric_limits<double>::infinity(); }
        var getWithInts (int64 a, int64 b) const override       { return b != 0 ? var ((double) a / (double) b) : var (std::numeric_limits<double>::infinity()); }
    };

    struct ModuloOp  : public BinaryOperator
    {
        ModuloOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::modulo) {}
        var getWithDoubles (double a, double b) const override  { return b != 0 ? fmod (a, b) : std::numeric_limits<double>::infinity(); }
        var getWithInts (int64 a, int64 b) const override       { return b != 0 ? var (a % b) : var (std::numeric_limits<double>::infinity()); }
    };

    struct BitwiseOrOp  : public BinaryOperator
    {
        BitwiseOrOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::bitwiseOr) {}
        var getWithInts (int64 a, int64 b) const override   { return a | b; }
    };

    struct BitwiseAndOp  : public BinaryOperator
    {
        BitwiseAndOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::bitwiseAnd) {}
        var getWithInts (int64 a, int64 b) const override   { return a & b; }
    };

    struct BitwiseXorOp  : public BinaryOperator
    {
        BitwiseXorOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::bitwiseXor) {}
        var getWithInts (int64 a, int64 b) const override   { return a ^ b; }
    };

    struct LeftShiftOp  : public BinaryOperator
    {
        LeftShiftOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::leftShift) {}
        var getWithInts (int64 a, int64 b) const override   { return ((int) a) << (int) b; }
    };

    struct RightShiftOp  : public BinaryOperator
    {
        RightShiftOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::rightShift) {}
        var getWithInts (int64 a, int64 b) const override   { return ((int) a) >> (int) b; }
    };

    struct RightShiftUnsignedOp  : public BinaryOperator
    {
        RightShiftUnsignedOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperator (l, a, b, TokenTypes::rightShiftUnsigned) {}
        var getWithInts (int64 a, int64 b) const override   { return (int) (((uint32) a) >> (int) b); }
    };

    struct LogicalAndOp  : public BinaryOperatorBase
    {
        LogicalAndOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperatorBase (l, a, b, TokenTypes::logicalAnd) {}
        var getResult (const Scope& s) const override       { return lhs->getResult (s) && rhs->getResult (s); }
    };

    struct LogicalOrOp  : public BinaryOperatorBase
    {
        LogicalOrOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperatorBase (l, a, b, TokenTypes::logicalOr) {}
        var getResult (const Scope& s) const override       { return lhs->getResult (s) || rhs->getResult (s); }
    };

    struct TypeEqualsOp  : public BinaryOperatorBase
    {
        TypeEqualsOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperatorBase (l, a, b, TokenTypes::typeEquals) {}
        var getResult (const Scope& s) const override       { return areTypeEqual (lhs->getResult (s), rhs->getResult (s)); }
    };

    struct TypeNotEqualsOp  : public BinaryOperatorBase
    {
        TypeNotEqualsOp (const CodeLocation& l, ExpPtr& a, ExpPtr& b) noexcept : BinaryOperatorBase (l, a, b, TokenTypes::typeNotEquals) {}
        var getResult (const Scope& s) const override       { return ! areTypeEqual (lhs->getResult (s), rhs->getResult (s)); }
    };

    struct ConditionalOp  : public Expression
    {
        ConditionalOp (const CodeLocation& l) noexcept : Expression (l) {}

        var getResult (const Scope& s) const override              { return (condition->getResult (s) ? trueBranch : falseBranch)->getResult (s); }
        void assign (const Scope& s, const var& v) const override  { (condition->getResult (s) ? trueBranch : falseBranch)->assign (s, v); }

        ExpPtr condition, trueBranch, falseBranch;
    };

    struct Assignment  : public Expression
    {
        Assignment (const CodeLocation& l, ExpPtr& dest, ExpPtr& source) noexcept : Expression (l), target (dest.release()), newValue (source.release()) {}

        var getResult (const Scope& s) const override
        {
            auto value = newValue->getResult (s);
            target->assign (s, value);
            return value;
        }

        ExpPtr target, newValue;
    };

    struct SelfAssignment  : public Expression
    {
        SelfAssignment (const CodeLocation& l, Expression* dest, Expression* source) noexcept
            : Expression (l), target (dest), newValue (source) {}

        var getResult (const Scope& s) const override
        {
            auto value = newValue->getResult (s);
            target->assign (s, value);
            return value;
        }

        Expression* target; // Careful! this pointer aliases a sub-term of newValue!
        ExpPtr newValue;
        TokenType op;
    };

    struct PostAssignment  : public SelfAssignment
    {
        PostAssignment (const CodeLocation& l, Expression* dest, Expression* source) noexcept : SelfAssignment (l, dest, source) {}

        var getResult (const Scope& s) const override
        {
            auto oldValue = target->getResult (s);
            target->assign (s, newValue->getResult (s));
            return oldValue;
        }
    };

    struct FunctionCall  : public Expression
    {
        FunctionCall (const CodeLocation& l) noexcept : Expression (l) {}

        var getResult (const Scope& s) const override
        {
            if (auto* dot = dynamic_cast<DotOperator*> (object.get()))
            {
                auto thisObject = dot->parent->getResult (s);
                return invokeFunction (s, s.findFunctionCall (location, thisObject, dot->child), thisObject);
            }

            auto function = object->getResult (s);
            return invokeFunction (s, function, var (s.scope.get()));
        }

        var invokeFunction (const Scope& s, const var& function, const var& thisObject) const
        {
            s.checkTimeOut (location);
            Array<var> argVars;

            for (auto* a : arguments)
                argVars.add (a->getResult (s));

            const var::NativeFunctionArgs args (thisObject, argVars.begin(), argVars.size());

            if (var::NativeFunction nativeFunction = function.getNativeFunction())
                return nativeFunction (args);

            if (auto* fo = dynamic_cast<FunctionObject*> (function.getObject()))
                return fo->invoke (s, args);

            if (auto* dot = dynamic_cast<DotOperator*> (object.get()))
                if (auto* o = thisObject.getDynamicObject())
                    if (o->hasMethod (dot->child)) // allow an overridden DynamicObject::invokeMethod to accept a method call.
                        return o->invokeMethod (dot->child, args);

            location.throwError ("This expression is not a function!"); return {};
        }

        ExpPtr object;
        OwnedArray<Expression> arguments;
    };

    struct NewOperator  : public FunctionCall
    {
        NewOperator (const CodeLocation& l) noexcept : FunctionCall (l) {}

        var getResult (const Scope& s) const override
        {
            var classOrFunc = object->getResult (s);
            const bool isFunc = isFunction (classOrFunc);

            if (! (isFunc || classOrFunc.getDynamicObject() != nullptr))
                return var::undefined();

            DynamicObject::Ptr newObject (new DynamicObject());

            if (isFunc)
                invokeFunction (s, classOrFunc, newObject.get());
            else
                newObject->setProperty (getPrototypeIdentifier(), classOrFunc);

            return newObject.get();
        }
    };

    struct ObjectDeclaration  : public Expression
    {
        ObjectDeclaration (const CodeLocation& l) noexcept : Expression (l) {}

        var getResult (const Scope& s) const override
        {
            DynamicObject::Ptr newObject (new DynamicObject());

            for (int i = 0; i < names.size(); ++i)
                newObject->setProperty (names.getUnchecked(i), initialisers.getUnchecked(i)->getResult (s));

            return newObject.get();
        }

        Array<Identifier> names;
        OwnedArray<Expression> initialisers;
    };

    struct ArrayDeclaration  : public Expression
    {
        ArrayDeclaration (const CodeLocation& l) noexcept : Expression (l) {}

        var getResult (const Scope& s) const override
        {
            Array<var> a;

            for (int i = 0; i < values.size(); ++i)
                a.add (values.getUnchecked(i)->getResult (s));

            // std::move() needed here for older compilers
            JUCE_BEGIN_IGNORE_WARNINGS_GCC_LIKE ("-Wredundant-move")
            return std::move (a);
            JUCE_END_IGNORE_WARNINGS_GCC_LIKE
        }

        OwnedArray<Expression> values;
    };

    //==============================================================================
    struct FunctionObject  : public DynamicObject
    {
        FunctionObject() noexcept {}

        FunctionObject (const FunctionObject& other)  : DynamicObject(), functionCode (other.functionCode)
        {
            ExpressionTreeBuilder tb (functionCode);
            tb.parseFunctionParamsAndBody (*this);
        }

        DynamicObject::Ptr clone() override    { return *new FunctionObject (*this); }

        void writeAsJSON (OutputStream& out, int /*indentLevel*/, bool /*allOnOneLine*/, int /*maximumDecimalPlaces*/) override
        {
            out << "function " << functionCode;
        }

        var invoke (const Scope& s, const var::NativeFunctionArgs& args) const
        {
            DynamicObject::Ptr functionRoot (new DynamicObject());

            static const Identifier thisIdent ("this");
            functionRoot->setProperty (thisIdent, args.thisObject);

            for (int i = 0; i < parameters.size(); ++i)
                functionRoot->setProperty (parameters.getReference(i),
                                           i < args.numArguments ? args.arguments[i] : var::undefined());

            var result;
            body->perform (Scope (&s, s.root, functionRoot), &result);
            return result;
        }

        String functionCode;
        Array<Identifier> parameters;
        std::unique_ptr<Statement> body;
    };

    //==============================================================================
    struct TokenIterator
    {
        TokenIterator (const String& code) : location (code), p (code.getCharPointer()) { skip(); }

        void skip()
        {
            skipWhitespaceAndComments();
            location.location = p;
            currentType = matchNextToken();
        }

        void match (TokenType expected)
        {
            if (currentType != expected)
                location.throwError ("Found " + getTokenName (currentType) + " when expecting " + getTokenName (expected));

            skip();
        }

        bool matchIf (TokenType expected)                                 { if (currentType == expected)  { skip(); return true; } return false; }
        bool matchesAny (TokenType t1, TokenType t2) const                { return currentType == t1 || currentType == t2; }
        bool matchesAny (TokenType t1, TokenType t2, TokenType t3) const  { return matchesAny (t1, t2) || currentType == t3; }

        CodeLocation location;
        TokenType currentType;
        var currentValue;

    private:
        String::CharPointerType p;

        static bool isIdentifierStart (juce_wchar c) noexcept   { return CharacterFunctions::isLetter (c)        || c == '_'; }
        static bool isIdentifierBody  (juce_wchar c) noexcept   { return CharacterFunctions::isLetterOrDigit (c) || c == '_'; }

        TokenType matchNextToken()
        {
            if (isIdentifierStart (*p))
            {
                auto end = p;
                while (isIdentifierBody (*++end)) {}

                auto len = (size_t) (end - p);
                #define JUCE_JS_COMPARE_KEYWORD(name, str) if (len == sizeof (str) - 1 && matchToken (TokenTypes::name, len)) return TokenTypes::name;
                JUCE_JS_KEYWORDS (JUCE_JS_COMPARE_KEYWORD)

                currentValue = String (p, end); p = end;
                return TokenTypes::identifier;
            }

            if (p.isDigit())
            {
                if (parseHexLiteral() || parseFloatLiteral() || parseOctalLiteral() || parseDecimalLiteral())
                    return TokenTypes::literal;

                location.throwError ("Syntax error in numeric constant");
            }

            if (parseStringLiteral (*p) || (*p == '.' && parseFloatLiteral()))
                return TokenTypes::literal;

            #define JUCE_JS_COMPARE_OPERATOR(name, str) if (matchToken (TokenTypes::name, sizeof (str) - 1)) return TokenTypes::name;
            JUCE_JS_OPERATORS (JUCE_JS_COMPARE_OPERATOR)

            if (! p.isEmpty())
                location.throwError ("Unexpected character '" + String::charToString (*p) + "' in source");

            return TokenTypes::eof;
        }

        bool matchToken (TokenType name, size_t len) noexcept
        {
            if (p.compareUpTo (CharPointer_ASCII (name), (int) len) != 0) return false;
            p += (int) len;  return true;
        }

        void skipWhitespaceAndComments()
        {
            for (;;)
            {
                p = p.findEndOfWhitespace();

                if (*p == '/')
                {
                    auto c2 = p[1];

                    if (c2 == '/')  { p = CharacterFunctions::find (p, (juce_wchar) '\n'); continue; }

                    if (c2 == '*')
                    {
                        location.location = p;
                        p = CharacterFunctions::find (p + 2, CharPointer_ASCII ("*/"));
                        if (p.isEmpty()) location.throwError ("Unterminated '/*' comment");
                        p += 2; continue;
                    }
                }

                break;
            }
        }

        bool parseStringLiteral (juce_wchar quoteType)
        {
            if (quoteType != '"' && quoteType != '\'')
                return false;

            auto r = JSON::parseQuotedString (p, currentValue);
            if (r.failed()) location.throwError (r.getErrorMessage());
            return true;
        }

        bool parseHexLiteral()
        {
            if (*p != '0' || (p[1] != 'x' && p[1] != 'X')) return false;

            auto t = ++p;
            int64 v = CharacterFunctions::getHexDigitValue (*++t);
            if (v < 0) return false;

            for (;;)
            {
                auto digit = CharacterFunctions::getHexDigitValue (*++t);
                if (digit < 0) break;
                v = v * 16 + digit;
            }

            currentValue = v; p = t;
            return true;
        }

        bool parseFloatLiteral()
        {
            int numDigits = 0;
            auto t = p;
            while (t.isDigit())  { ++t; ++numDigits; }

            const bool hasPoint = (*t == '.');

            if (hasPoint)
                while ((++t).isDigit())  ++numDigits;

            if (numDigits == 0)
                return false;

            auto c = *t;
            const bool hasExponent = (c == 'e' || c == 'E');

            if (hasExponent)
            {
                c = *++t;
                if (c == '+' || c == '-')  ++t;
                if (! t.isDigit()) return false;
                while ((++t).isDigit()) {}
            }

            if (! (hasExponent || hasPoint)) return false;

            currentValue = CharacterFunctions::getDoubleValue (p);  p = t;
            return true;
        }

        bool parseOctalLiteral()
        {
            auto t = p;
            int64 v = *t - '0';
            if (v != 0) return false;  // first digit of octal must be 0

            for (;;)
            {
                auto digit = (int) (*++t - '0');
                if (isPositiveAndBelow (digit, 8))        v = v * 8 + digit;
                else if (isPositiveAndBelow (digit, 10))  location.throwError ("Decimal digit in octal constant");
                else break;
            }

            currentValue = v;  p = t;
            return true;
        }

        bool parseDecimalLiteral()
        {
            int64 v = 0;

            for (;; ++p)
            {
                auto digit = (int) (*p - '0');
                if (isPositiveAndBelow (digit, 10))  v = v * 10 + digit;
                else break;
            }

            currentValue = v;
            return true;
        }
    };

    //==============================================================================
    struct ExpressionTreeBuilder  : private TokenIterator
    {
        ExpressionTreeBuilder (const String code)  : TokenIterator (code) {}

        BlockStatement* parseStatementList()
        {
            std::unique_ptr<BlockStatement> b (new BlockStatement (location));

            while (currentType != TokenTypes::closeBrace && currentType != TokenTypes::eof)
                b->statements.add (parseStatement());

            return b.release();
        }

        void parseFunctionParamsAndBody (FunctionObject& fo)
        {
            match (TokenTypes::openParen);

            while (currentType != TokenTypes::closeParen)
            {
                auto paramName = currentValue.toString();
                match (TokenTypes::identifier);
                fo.parameters.add (paramName);

                if (currentType != TokenTypes::closeParen)
                    match (TokenTypes::comma);
            }

            match (TokenTypes::closeParen);
            fo.body.reset (parseBlock());
        }

        Expression* parseExpression()
        {
            ExpPtr lhs (parseLogicOperator());

            if (matchIf (TokenTypes::question))          return parseTernaryOperator (lhs);
            if (matchIf (TokenTypes::assign))            { ExpPtr rhs (parseExpression()); return new Assignment (location, lhs, rhs); }
            if (matchIf (TokenTypes::plusEquals))        return parseInPlaceOpExpression<AdditionOp> (lhs);
            if (matchIf (TokenTypes::minusEquals))       return parseInPlaceOpExpression<SubtractionOp> (lhs);
            if (matchIf (TokenTypes::timesEquals))       return parseInPlaceOpExpression<MultiplyOp> (lhs);
            if (matchIf (TokenTypes::divideEquals))      return parseInPlaceOpExpression<DivideOp> (lhs);
            if (matchIf (TokenTypes::moduloEquals))      return parseInPlaceOpExpression<ModuloOp> (lhs);
            if (matchIf (TokenTypes::leftShiftEquals))   return parseInPlaceOpExpression<LeftShiftOp> (lhs);
            if (matchIf (TokenTypes::rightShiftEquals))  return parseInPlaceOpExpression<RightShiftOp> (lhs);

            return lhs.release();
        }

    private:
        void throwError (const String& err) const  { location.throwError (err); }

        template <typename OpType>
        Expression* parseInPlaceOpExpression (ExpPtr& lhs)
        {
            ExpPtr rhs (parseExpression());
            Expression* bareLHS = lhs.get(); // careful - bare pointer is deliberately aliased
            return new SelfAssignment (location, bareLHS, new OpType (location, lhs, rhs));
        }

        BlockStatement* parseBlock()
        {
            match (TokenTypes::openBrace);
            std::unique_ptr<BlockStatement> b (parseStatementList());
            match (TokenTypes::closeBrace);
            return b.release();
        }

        Statement* parseStatement()
        {
            if (currentType == TokenTypes::openBrace)   return parseBlock();
            if (matchIf (TokenTypes::var))              return parseVar();
            if (matchIf (TokenTypes::if_))              return parseIf();
            if (matchIf (TokenTypes::while_))           return parseDoOrWhileLoop (false);
            if (matchIf (TokenTypes::do_))              return parseDoOrWhileLoop (true);
            if (matchIf (TokenTypes::for_))             return parseForLoop();
            if (matchIf (TokenTypes::return_))          return parseReturn();
            if (matchIf (TokenTypes::break_))           return new BreakStatement (location);
            if (matchIf (TokenTypes::continue_))        return new ContinueStatement (location);
            if (matchIf (TokenTypes::function))         return parseFunction();
            if (matchIf (TokenTypes::semicolon))        return new Statement (location);
            if (matchIf (TokenTypes::plusplus))         return parsePreIncDec<AdditionOp>();
            if (matchIf (TokenTypes::minusminus))       return parsePreIncDec<SubtractionOp>();

            if (matchesAny (TokenTypes::openParen, TokenTypes::openBracket))
                return matchEndOfStatement (parseFactor());

            if (matchesAny (TokenTypes::identifier, TokenTypes::literal, TokenTypes::minus))
                return matchEndOfStatement (parseExpression());

            throwError ("Found " + getTokenName (currentType) + " when expecting a statement");
            return nullptr;
        }

        Expression* matchEndOfStatement (Expression* ex)  { ExpPtr e (ex); if (currentType != TokenTypes::eof) match (TokenTypes::semicolon); return e.release(); }
        Expression* matchCloseParen (Expression* ex)      { ExpPtr e (ex); match (TokenTypes::closeParen); return e.release(); }

        Statement* parseIf()
        {
            std::unique_ptr<IfStatement> s (new IfStatement (location));
            match (TokenTypes::openParen);
            s->condition.reset (parseExpression());
            match (TokenTypes::closeParen);
            s->trueBranch.reset (parseStatement());
            s->falseBranch.reset (matchIf (TokenTypes::else_) ? parseStatement() : new Statement (location));
            return s.release();
        }

        Statement* parseReturn()
        {
            if (matchIf (TokenTypes::semicolon))
                return new ReturnStatement (location, new Expression (location));

            auto* r = new ReturnStatement (location, parseExpression());
            matchIf (TokenTypes::semicolon);
            return r;
        }

        Statement* parseVar()
        {
            std::unique_ptr<VarStatement> s (new VarStatement (location));
            s->name = parseIdentifier();
            s->initialiser.reset (matchIf (TokenTypes::assign) ? parseExpression() : new Expression (location));

            if (matchIf (TokenTypes::comma))
            {
                std::unique_ptr<BlockStatement> block (new BlockStatement (location));
                block->statements.add (std::move (s));
                block->statements.add (parseVar());
                return block.release();
            }

            match (TokenTypes::semicolon);
            return s.release();
        }

        Statement* parseFunction()
        {
            Identifier name;
            auto fn = parseFunctionDefinition (name);

            if (name.isNull())
                throwError ("Functions defined at statement-level must have a name");

            ExpPtr nm (new UnqualifiedName (location, name)), value (new LiteralValue (location, fn));
            return new Assignment (location, nm, value);
        }

        Statement* parseForLoop()
        {
            std::unique_ptr<LoopStatement> s (new LoopStatement (location, false));
            match (TokenTypes::openParen);
            s->initialiser.reset (parseStatement());

            if (matchIf (TokenTypes::semicolon))
                s->condition.reset (new LiteralValue (location, true));
            else
            {
                s->condition.reset (parseExpression());
                match (TokenTypes::semicolon);
            }

            if (matchIf (TokenTypes::closeParen))
                s->iterator.reset (new Statement (location));
            else
            {
                s->iterator.reset (parseExpression());
                match (TokenTypes::closeParen);
            }

            s->body.reset (parseStatement());
            return s.release();
        }

        Statement* parseDoOrWhileLoop (bool isDoLoop)
        {
            std::unique_ptr<LoopStatement> s (new LoopStatement (location, isDoLoop));
            s->initialiser.reset (new Statement (location));
            s->iterator.reset (new Statement (location));

            if (isDoLoop)
            {
                s->body.reset (parseBlock());
                match (TokenTypes::while_);
            }

            match (TokenTypes::openParen);
            s->condition.reset (parseExpression());
            match (TokenTypes::closeParen);

            if (! isDoLoop)
                s->body.reset (parseStatement());

            return s.release();
        }

        Identifier parseIdentifier()
        {
            Identifier i;
            if (currentType == TokenTypes::identifier)
                i = currentValue.toString();

            match (TokenTypes::identifier);
            return i;
        }

        var parseFunctionDefinition (Identifier& functionName)
        {
            auto functionStart = location.location;

            if (currentType == TokenTypes::identifier)
                functionName = parseIdentifier();

            std::unique_ptr<FunctionObject> fo (new FunctionObject());
            parseFunctionParamsAndBody (*fo);
            fo->functionCode = String (functionStart, location.location);
            return var (fo.release());
        }

        Expression* parseFunctionCall (FunctionCall* call, ExpPtr& function)
        {
            std::unique_ptr<FunctionCall> s (call);
            s->object.reset (function.release());
            match (TokenTypes::openParen);

            while (currentType != TokenTypes::closeParen)
            {
                s->arguments.add (parseExpression());
                if (currentType != TokenTypes::closeParen)
                    match (TokenTypes::comma);
            }

            return matchCloseParen (s.release());
        }

        Expression* parseSuffixes (Expression* e)
        {
            ExpPtr input (e);

            if (matchIf (TokenTypes::dot))
                return parseSuffixes (new DotOperator (location, input, parseIdentifier()));

            if (currentType == TokenTypes::openParen)
                return parseSuffixes (parseFunctionCall (new FunctionCall (location), input));

            if (matchIf (TokenTypes::openBracket))
            {
                std::unique_ptr<ArraySubscript> s (new ArraySubscript (location));
                s->object.reset (input.release());
                s->index.reset (parseExpression());
                match (TokenTypes::closeBracket);
                return parseSuffixes (s.release());
            }

            if (matchIf (TokenTypes::plusplus))   return parsePostIncDec<AdditionOp> (input);
            if (matchIf (TokenTypes::minusminus)) return parsePostIncDec<SubtractionOp> (input);

            return input.release();
        }

        Expression* parseFactor()
        {
            if (currentType == TokenTypes::identifier)  return parseSuffixes (new UnqualifiedName (location, parseIdentifier()));
            if (matchIf (TokenTypes::openParen))        return parseSuffixes (matchCloseParen (parseExpression()));
            if (matchIf (TokenTypes::true_))            return parseSuffixes (new LiteralValue (location, (int) 1));
            if (matchIf (TokenTypes::false_))           return parseSuffixes (new LiteralValue (location, (int) 0));
            if (matchIf (TokenTypes::null_))            return parseSuffixes (new LiteralValue (location, var()));
            if (matchIf (TokenTypes::undefined))        return parseSuffixes (new Expression (location));

            if (currentType == TokenTypes::literal)
            {
                var v (currentValue); skip();
                return parseSuffixes (new LiteralValue (location, v));
            }

            if (matchIf (TokenTypes::openBrace))
            {
                std::unique_ptr<ObjectDeclaration> e (new ObjectDeclaration (location));

                while (currentType != TokenTypes::closeBrace)
                {
                    auto memberName = currentValue.toString();
                    match ((currentType == TokenTypes::literal && currentValue.isString())
                             ? TokenTypes::literal : TokenTypes::identifier);
                    match (TokenTypes::colon);

                    e->names.add (memberName);
                    e->initialisers.add (parseExpression());

                    if (currentType != TokenTypes::closeBrace)
                        match (TokenTypes::comma);
                }

                match (TokenTypes::closeBrace);
                return parseSuffixes (e.release());
            }

            if (matchIf (TokenTypes::openBracket))
            {
                std::unique_ptr<ArrayDeclaration> e (new ArrayDeclaration (location));

                while (currentType != TokenTypes::closeBracket)
                {
                    e->values.add (parseExpression());

                    if (currentType != TokenTypes::closeBracket)
                        match (TokenTypes::comma);
                }

                match (TokenTypes::closeBracket);
                return parseSuffixes (e.release());
            }

            if (matchIf (TokenTypes::function))
            {
                Identifier name;
                var fn = parseFunctionDefinition (name);

                if (name.isValid())
                    throwError ("Inline functions definitions cannot have a name");

                return new LiteralValue (location, fn);
            }

            if (matchIf (TokenTypes::new_))
            {
                ExpPtr name (new UnqualifiedName (location, parseIdentifier()));

                while (matchIf (TokenTypes::dot))
                    name.reset (new DotOperator (location, name, parseIdentifier()));

                return parseFunctionCall (new NewOperator (location), name);
            }

            throwError ("Found " + getTokenName (currentType) + " when expecting an expression");
            return nullptr;
        }

        template <typename OpType>
        Expression* parsePreIncDec()
        {
            Expression* e = parseFactor(); // careful - bare pointer is deliberately aliased
            ExpPtr lhs (e), one (new LiteralValue (location, (int) 1));
            return new SelfAssignment (location, e, new OpType (location, lhs, one));
        }

        template <typename OpType>
        Expression* parsePostIncDec (ExpPtr& lhs)
        {
            Expression* e = lhs.release(); // careful - bare pointer is deliberately aliased
            ExpPtr lhs2 (e), one (new LiteralValue (location, (int) 1));
            return new PostAssignment (location, e, new OpType (location, lhs2, one));
        }

        Expression* parseTypeof()
        {
            std::unique_ptr<FunctionCall> f (new FunctionCall (location));
            f->object.reset (new UnqualifiedName (location, "typeof"));
            f->arguments.add (parseUnary());
            return f.release();
        }

        Expression* parseUnary()
        {
            if (matchIf (TokenTypes::minus))       { ExpPtr a (new LiteralValue (location, (int) 0)), b (parseUnary()); return new SubtractionOp   (location, a, b); }
            if (matchIf (TokenTypes::logicalNot))  { ExpPtr a (new LiteralValue (location, (int) 0)), b (parseUnary()); return new EqualsOp        (location, a, b); }
            if (matchIf (TokenTypes::plusplus))    return parsePreIncDec<AdditionOp>();
            if (matchIf (TokenTypes::minusminus))  return parsePreIncDec<SubtractionOp>();
            if (matchIf (TokenTypes::typeof_))     return parseTypeof();

            return parseFactor();
        }

        Expression* parseMultiplyDivide()
        {
            ExpPtr a (parseUnary());

            for (;;)
            {
                if (matchIf (TokenTypes::times))        { ExpPtr b (parseUnary()); a.reset (new MultiplyOp (location, a, b)); }
                else if (matchIf (TokenTypes::divide))  { ExpPtr b (parseUnary()); a.reset (new DivideOp   (location, a, b)); }
                else if (matchIf (TokenTypes::modulo))  { ExpPtr b (parseUnary()); a.reset (new ModuloOp   (location, a, b)); }
                else break;
            }

            return a.release();
        }

        Expression* parseAdditionSubtraction()
        {
            ExpPtr a (parseMultiplyDivide());

            for (;;)
            {
                if (matchIf (TokenTypes::plus))            { ExpPtr b (parseMultiplyDivide()); a.reset (new AdditionOp    (location, a, b)); }
                else if (matchIf (TokenTypes::minus))      { ExpPtr b (parseMultiplyDivide()); a.reset (new SubtractionOp (location, a, b)); }
                else break;
            }

            return a.release();
        }

        Expression* parseShiftOperator()
        {
            ExpPtr a (parseAdditionSubtraction());

            for (;;)
            {
                if (matchIf (TokenTypes::leftShift))                { ExpPtr b (parseExpression()); a.reset (new LeftShiftOp          (location, a, b)); }
                else if (matchIf (TokenTypes::rightShift))          { ExpPtr b (parseExpression()); a.reset (new RightShiftOp         (location, a, b)); }
                else if (matchIf (TokenTypes::rightShiftUnsigned))  { ExpPtr b (parseExpression()); a.reset (new RightShiftUnsignedOp (location, a, b)); }
                else break;
            }

            return a.release();
        }

        Expression* parseComparator()
        {
            ExpPtr a (parseShiftOperator());

            for (;;)
            {
                if (matchIf (TokenTypes::equals))                  { ExpPtr b (parseShiftOperator()); a.reset (new EqualsOp             (location, a, b)); }
                else if (matchIf (TokenTypes::notEquals))          { ExpPtr b (parseShiftOperator()); a.reset (new NotEqualsOp          (location, a, b)); }
                else if (matchIf (TokenTypes::typeEquals))         { ExpPtr b (parseShiftOperator()); a.reset (new TypeEqualsOp         (location, a, b)); }
                else if (matchIf (TokenTypes::typeNotEquals))      { ExpPtr b (parseShiftOperator()); a.reset (new TypeNotEqualsOp      (location, a, b)); }
                else if (matchIf (TokenTypes::lessThan))           { ExpPtr b (parseShiftOperator()); a.reset (new LessThanOp           (location, a, b)); }
                else if (matchIf (TokenTypes::lessThanOrEqual))    { ExpPtr b (parseShiftOperator()); a.reset (new LessThanOrEqualOp    (location, a, b)); }
                else if (matchIf (TokenTypes::greaterThan))        { ExpPtr b (parseShiftOperator()); a.reset (new GreaterThanOp        (location, a, b)); }
                else if (matchIf (TokenTypes::greaterThanOrEqual)) { ExpPtr b (parseShiftOperator()); a.reset (new GreaterThanOrEqualOp (location, a, b)); }
                else break;
            }

            return a.release();
        }

        Expression* parseLogicOperator()
        {
            ExpPtr a (parseComparator());

            for (;;)
            {
                if (matchIf (TokenTypes::logicalAnd))       { ExpPtr b (parseComparator()); a.reset (new LogicalAndOp (location, a, b)); }
                else if (matchIf (TokenTypes::logicalOr))   { ExpPtr b (parseComparator()); a.reset (new LogicalOrOp  (location, a, b)); }
                else if (matchIf (TokenTypes::bitwiseAnd))  { ExpPtr b (parseComparator()); a.reset (new BitwiseAndOp (location, a, b)); }
                else if (matchIf (TokenTypes::bitwiseOr))   { ExpPtr b (parseComparator()); a.reset (new BitwiseOrOp  (location, a, b)); }
                else if (matchIf (TokenTypes::bitwiseXor))  { ExpPtr b (parseComparator()); a.reset (new BitwiseXorOp (location, a, b)); }
                else break;
            }

            return a.release();
        }

        Expression* parseTernaryOperator (ExpPtr& condition)
        {
            std::unique_ptr<ConditionalOp> e (new ConditionalOp (location));
            e->condition.reset (condition.release());
            e->trueBranch.reset (parseExpression());
            match (TokenTypes::colon);
            e->falseBranch.reset (parseExpression());
            return e.release();
        }

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (ExpressionTreeBuilder)
    };

    //==============================================================================
    static var get (Args a, int index) noexcept            { return index < a.numArguments ? a.arguments[index] : var(); }
    static bool isInt (Args a, int index) noexcept         { return get (a, index).isInt() || get (a, index).isInt64(); }
    static int getInt (Args a, int index) noexcept         { return get (a, index); }
    static double getDouble (Args a, int index) noexcept   { return get (a, index); }
    static String getString (Args a, int index) noexcept   { return get (a, index).toString(); }

    //==============================================================================
    struct ObjectClass  : public DynamicObject
    {
        ObjectClass()
        {
            setMethod ("dump",  dump);
            setMethod ("clone", cloneFn);
        }

        static Identifier getClassName()   { static const Identifier i ("Object"); return i; }
        static var dump  (Args a)          { DBG (JSON::toString (a.thisObject)); ignoreUnused (a); return var::undefined(); }
        static var cloneFn (Args a)        { return a.thisObject.clone(); }
    };

    //==============================================================================
    struct ArrayClass  : public DynamicObject
    {
        ArrayClass()
        {
            setMethod ("contains", contains);
            setMethod ("remove",   remove);
            setMethod ("join",     join);
            setMethod ("push",     push);
            setMethod ("splice",   splice);
            setMethod ("indexOf",  indexOf);
        }

        static Identifier getClassName()   { static const Identifier i ("Array"); return i; }

        static var contains (Args a)
        {
            if (auto* array = a.thisObject.getArray())
                return array->contains (get (a, 0));

            return false;
        }

        static var remove (Args a)
        {
            if (auto* array = a.thisObject.getArray())
                array->removeAllInstancesOf (get (a, 0));

            return var::undefined();
        }

        static var join (Args a)
        {
            StringArray strings;

            if (auto* array = a.thisObject.getArray())
                for (auto& v : *array)
                    strings.add (v.toString());

            return strings.joinIntoString (getString (a, 0));
        }

        static var push (Args a)
        {
            if (auto* array = a.thisObject.getArray())
            {
                for (int i = 0; i < a.numArguments; ++i)
                    array->add (a.arguments[i]);

                return array->size();
            }

            return var::undefined();
        }

        static var splice (Args a)
        {
            if (auto* array = a.thisObject.getArray())
            {
                auto arraySize = array->size();
                int start = get (a, 0);

                if (start < 0)
                    start = jmax (0, arraySize + start);
                else if (start > arraySize)
                    start = arraySize;

                const int num = a.numArguments > 1 ? jlimit (0, arraySize - start, getInt (a, 1))
                                                   : arraySize - start;

                Array<var> itemsRemoved;
                itemsRemoved.ensureStorageAllocated (num);

                for (int i = 0; i < num; ++i)
                    itemsRemoved.add (array->getReference (start + i));

                array->removeRange (start, num);

                for (int i = 2; i < a.numArguments; ++i)
                    array->insert (start++, get (a, i));

                // std::move() needed here for older compilers
                JUCE_BEGIN_IGNORE_WARNINGS_GCC_LIKE ("-Wredundant-move")
                return std::move (itemsRemoved);
                JUCE_END_IGNORE_WARNINGS_GCC_LIKE
            }

            return var::undefined();
        }

        static var indexOf (Args a)
        {
            if (auto* array = a.thisObject.getArray())
            {
                auto target = get (a, 0);

                for (int i = (a.numArguments > 1 ? getInt (a, 1) : 0); i < array->size(); ++i)
                    if (array->getReference(i) == target)
                        return i;
            }

            return -1;
        }
    };

    //==============================================================================
    struct StringClass  : public DynamicObject
    {
        StringClass()
        {
            setMethod ("substring",     substring);
            setMethod ("indexOf",       indexOf);
            setMethod ("charAt",        charAt);
            setMethod ("charCodeAt",    charCodeAt);
            setMethod ("fromCharCode",  fromCharCode);
            setMethod ("split",         split);
        }

        static Identifier getClassName()  { static const Identifier i ("String"); return i; }

        static var fromCharCode (Args a)  { return String::charToString (static_cast<juce_wchar> (getInt (a, 0))); }
        static var substring (Args a)     { return a.thisObject.toString().substring (getInt (a, 0), getInt (a, 1)); }
        static var indexOf (Args a)       { return a.thisObject.toString().indexOf (getString (a, 0)); }
        static var charCodeAt (Args a)    { return (int) a.thisObject.toString() [getInt (a, 0)]; }
        static var charAt (Args a)        { int p = getInt (a, 0); return a.thisObject.toString().substring (p, p + 1); }

        static var split (Args a)
        {
            auto str = a.thisObject.toString();
            auto sep = getString (a, 0);
            StringArray strings;

            if (sep.isNotEmpty())
                strings.addTokens (str, sep.substring (0, 1), {});
            else // special-case for empty separator: split all chars separately
                for (auto pos = str.getCharPointer(); ! pos.isEmpty(); ++pos)
                    strings.add (String::charToString (*pos));

            var array;

            for (auto& s : strings)
                array.append (s);

            return array;
        }
    };

    //==============================================================================
    struct MathClass  : public DynamicObject
    {
        MathClass()
        {
            setMethod ("abs",       Math_abs);              setMethod ("round",     Math_round);
            setMethod ("random",    Math_random);           setMethod ("randInt",   Math_randInt);
            setMethod ("min",       Math_min);              setMethod ("max",       Math_max);
            setMethod ("range",     Math_range);            setMethod ("sign",      Math_sign);
            setMethod ("toDegrees", Math_toDegrees);        setMethod ("toRadians", Math_toRadians);
            setMethod ("sin",       Math_sin);              setMethod ("asin",      Math_asin);
            setMethod ("sinh",      Math_sinh);             setMethod ("asinh",     Math_asinh);
            setMethod ("cos",       Math_cos);              setMethod ("acos",      Math_acos);
            setMethod ("cosh",      Math_cosh);             setMethod ("acosh",     Math_acosh);
            setMethod ("tan",       Math_tan);              setMethod ("atan",      Math_atan);
            setMethod ("tanh",      Math_tanh);             setMethod ("atanh",     Math_atanh);
            setMethod ("log",       Math_log);              setMethod ("log10",     Math_log10);
            setMethod ("exp",       Math_exp);              setMethod ("pow",       Math_pow);
            setMethod ("sqr",       Math_sqr);              setMethod ("sqrt",      Math_sqrt);
            setMethod ("ceil",      Math_ceil);             setMethod ("floor",     Math_floor);

            setProperty ("PI",      MathConstants<double>::pi);
            setProperty ("E",       MathConstants<double>::euler);
            setProperty ("SQRT2",   MathConstants<double>::sqrt2);
            setProperty ("SQRT1_2", std::sqrt (0.5));
            setProperty ("LN2",     std::log (2.0));
            setProperty ("LN10",    std::log (10.0));
            setProperty ("LOG2E",   std::log (MathConstants<double>::euler) / std::log (2.0));
            setProperty ("LOG10E",  std::log (MathConstants<double>::euler) / std::log (10.0));
        }

        static var Math_random    (Args)   { return Random::getSystemRandom().nextDouble(); }
        static var Math_randInt   (Args a) { return Random::getSystemRandom().nextInt (Range<int> (getInt (a, 0), getInt (a, 1))); }
        static var Math_abs       (Args a) { return isInt (a, 0) ? var (std::abs   (getInt (a, 0))) : var (std::abs   (getDouble (a, 0))); }
        static var Math_round     (Args a) { return isInt (a, 0) ? var (roundToInt (getInt (a, 0))) : var (roundToInt (getDouble (a, 0))); }
        static var Math_sign      (Args a) { return isInt (a, 0) ? var (sign       (getInt (a, 0))) : var (sign       (getDouble (a, 0))); }
        static var Math_range     (Args a) { return isInt (a, 0) ? var (jlimit (getInt (a, 1), getInt (a, 2), getInt (a, 0))) : var (jlimit (getDouble (a, 1), getDouble (a, 2), getDouble (a, 0))); }
        static var Math_min       (Args a) { return (isInt (a, 0) && isInt (a, 1)) ? var (jmin (getInt (a, 0), getInt (a, 1))) : var (jmin (getDouble (a, 0), getDouble (a, 1))); }
        static var Math_max       (Args a) { return (isInt (a, 0) && isInt (a, 1)) ? var (jmax (getInt (a, 0), getInt (a, 1))) : var (jmax (getDouble (a, 0), getDouble (a, 1))); }
        static var Math_toDegrees (Args a) { return radiansToDegrees (getDouble (a, 0)); }
        static var Math_toRadians (Args a) { return degreesToRadians (getDouble (a, 0)); }
        static var Math_sin       (Args a) { return std::sin   (getDouble (a, 0)); }
        static var Math_asin      (Args a) { return std::asin  (getDouble (a, 0)); }
        static var Math_cos       (Args a) { return std::cos   (getDouble (a, 0)); }
        static var Math_acos      (Args a) { return std::acos  (getDouble (a, 0)); }
        static var Math_sinh      (Args a) { return std::sinh  (getDouble (a, 0)); }
        static var Math_cosh      (Args a) { return std::cosh  (getDouble (a, 0)); }
        static var Math_tan       (Args a) { return std::tan   (getDouble (a, 0)); }
        static var Math_tanh      (Args a) { return std::tanh  (getDouble (a, 0)); }
        static var Math_atan      (Args a) { return std::atan  (getDouble (a, 0)); }
        static var Math_log       (Args a) { return std::log   (getDouble (a, 0)); }
        static var Math_log10     (Args a) { return std::log10 (getDouble (a, 0)); }
        static var Math_exp       (Args a) { return std::exp   (getDouble (a, 0)); }
        static var Math_pow       (Args a) { return std::pow   (getDouble (a, 0), getDouble (a, 1)); }
        static var Math_sqr       (Args a) { return square (getDouble (a, 0)); }
        static var Math_sqrt      (Args a) { return std::sqrt  (getDouble (a, 0)); }
        static var Math_ceil      (Args a) { return std::ceil  (getDouble (a, 0)); }
        static var Math_floor     (Args a) { return std::floor (getDouble (a, 0)); }

        // We can't use the std namespace equivalents of these functions without breaking
        // compatibility with older versions of OS X.
        static var Math_asinh     (Args a) { return asinh (getDouble (a, 0)); }
        static var Math_acosh     (Args a) { return acosh (getDouble (a, 0)); }
        static var Math_atanh     (Args a) { return atanh (getDouble (a, 0)); }

        static Identifier getClassName()   { static const Identifier i ("Math"); return i; }
        template <typename Type> static Type sign (Type n) noexcept  { return n > 0 ? (Type) 1 : (n < 0 ? (Type) -1 : 0); }
    };

    //==============================================================================
    struct JSONClass  : public DynamicObject
    {
        JSONClass()                        { setMethod ("stringify", stringify); }
        static Identifier getClassName()   { static const Identifier i ("JSON"); return i; }
        static var stringify (Args a)      { return JSON::toString (get (a, 0)); }
    };

    //==============================================================================
    struct IntegerClass  : public DynamicObject
    {
        IntegerClass()                     { setMethod ("parseInt",  parseInt); }
        static Identifier getClassName()   { static const Identifier i ("Integer"); return i; }

        static var parseInt (Args a)
        {
            auto s = getString (a, 0).trim();

            return s[0] == '0' ? (s[1] == 'x' ? s.substring(2).getHexValue64() : getOctalValue (s))
                               : s.getLargeIntValue();
        }
    };

    //==============================================================================
    static var trace (Args a)        { Logger::outputDebugString (JSON::toString (a.thisObject)); return var::undefined(); }
    static var charToInt (Args a)    { return (int) (getString (a, 0)[0]); }
    static var parseFloat (Args a)   { return getDouble (a, 0); }

    static var typeof_internal (Args a)
    {
        var v (get (a, 0));

        if (v.isVoid())                      return "void";
        if (v.isString())                    return "string";
        if (isNumeric (v))                   return "number";
        if (isFunction (v) || v.isMethod())  return "function";
        if (v.isObject())                    return "object";

        return "undefined";
    }

    static var exec (Args a)
    {
        if (auto* root = dynamic_cast<RootObject*> (a.thisObject.getObject()))
            root->execute (getString (a, 0));

        return var::undefined();
    }

    static var eval (Args a)
    {
        if (auto* root = dynamic_cast<RootObject*> (a.thisObject.getObject()))
            return root->evaluate (getString (a, 0));

        return var::undefined();
    }
};

//==============================================================================
JavascriptEngine::JavascriptEngine()  : maximumExecutionTime (15.0), root (new RootObject())
{
    registerNativeObject (RootObject::ObjectClass  ::getClassName(),  new RootObject::ObjectClass());
    registerNativeObject (RootObject::ArrayClass   ::getClassName(),  new RootObject::ArrayClass());
    registerNativeObject (RootObject::StringClass  ::getClassName(),  new RootObject::StringClass());
    registerNativeObject (RootObject::MathClass    ::getClassName(),  new RootObject::MathClass());
    registerNativeObject (RootObject::JSONClass    ::getClassName(),  new RootObject::JSONClass());
    registerNativeObject (RootObject::IntegerClass ::getClassName(),  new RootObject::IntegerClass());
}

JavascriptEngine::~JavascriptEngine() {}

void JavascriptEngine::prepareTimeout() const noexcept   { root->timeout = Time::getCurrentTime() + maximumExecutionTime; }
void JavascriptEngine::stop() noexcept                   { root->timeout = {}; }

void JavascriptEngine::registerNativeObject (const Identifier& name, DynamicObject* object)
{
    root->setProperty (name, object);
}

Result JavascriptEngine::execute (const String& code)
{
    try
    {
        prepareTimeout();
        root->execute (code);
    }
    catch (String& error)
    {
        return Result::fail (error);
    }

    return Result::ok();
}

var JavascriptEngine::evaluate (const String& code, Result* result)
{
    try
    {
        prepareTimeout();
        if (result != nullptr) *result = Result::ok();
        return root->evaluate (code);
    }
    catch (String& error)
    {
        if (result != nullptr) *result = Result::fail (error);
    }

    return var::undefined();
}

var JavascriptEngine::callFunction (const Identifier& function, const var::NativeFunctionArgs& args, Result* result)
{
    auto returnVal = var::undefined();

    try
    {
        prepareTimeout();
        if (result != nullptr) *result = Result::ok();
        RootObject::Scope ({}, *root, *root).findAndInvokeMethod (function, args, returnVal);
    }
    catch (String& error)
    {
        if (result != nullptr) *result = Result::fail (error);
    }

    return returnVal;
}

var JavascriptEngine::callFunctionObject (DynamicObject* objectScope, const var& functionObject,
                                          const var::NativeFunctionArgs& args, Result* result)
{
    auto returnVal = var::undefined();

    try
    {
        prepareTimeout();
        if (result != nullptr) *result = Result::ok();
        RootObject::Scope rootScope ({}, *root, *root);
        RootObject::Scope (&rootScope, *root, DynamicObject::Ptr (objectScope))
            .invokeMethod (functionObject, args, returnVal);
    }
    catch (String& error)
    {
        if (result != nullptr) *result = Result::fail (error);
    }

    return returnVal;
}

const NamedValueSet& JavascriptEngine::getRootObjectProperties() const noexcept
{
    return root->getProperties();
}

JUCE_END_IGNORE_WARNINGS_MSVC

} // namespace juce
