module expression;

import std.algorithm : map, find;
import std.conv : parse;
import std.format : formattedWrite;
import std.math : isInfinity;
import std.meta : AliasSeq;
import std.range : ElementType, iota, isInputRange, repeat;
import std.string : format, join;
import std.traits : arity, isSomeChar, isFloatingPoint;
import std.typecons : Nullable;
import std.uni : isAlpha, isAlphaNum, isNumber, isWhite;;
import std.utf : toUTF8;

class ExpressionError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }

    this(S)(ref const S source, string msg, string file = __FILE__, size_t line = __LINE__) {
        super(format("%s (%s)", msg, source.pos + 1), file, line);
    }
}

struct Expression(V) if(isFloatingPoint!V) {
    this() @disable;

    this(R)(R source) if(isInputRange!R && isSomeChar!(ElementType!R)) {
        auto src = Source!R(source);
        m_root = compileAdd(src, m_context);
        skipWS(src);
        if(!src.empty()) throw new ExpressionError(src, "syntax error");
    }

    V opCall() {
        validate();
        return m_root();
    }

    void toString(W)(ref W writer) {
        void writeNode(const Node!V node, string indent = "") {
            writer.formattedWrite("%s%s\n", indent, node);
            foreach(child; node.children) writeNode(child, indent ~ "  ");
        }
        writeNode(m_root);
    }

    void opIndexAssign(V value, string name) {
        foreach(v; m_context.variables) {
            if(v.name == name) { v.set(value); return; }
        }
        throw new ExpressionError(format!"undefined variable (%s)"(name));
    }

    void opIndexAssign(F)(F fn, string name) {
        {
            // check function type;
            mixin("alias VS = AliasSeq!(" ~ "V.init".repeat(arity!F).join(",") ~ ");");
            V v;
            static assert(__traits(compiles, v = fn(VS)), format!"invalid function type (%s)"(F.stringof));
        }
        foreach(f; m_context.functions) {
            if(f.name == name && f.arity == arity!F) { f.set(fn); return; }
        }
        throw new ExpressionError(format!"undefined function (%s/%s)"(name, arity!F));
    }

    template opDispatch(string name) {
        void opDispatch(V)(V value) { this[name] = value; }
    }

    private {
        Node!V m_root;
        Context!V m_context;
        bool m_validated = false;

        void validate() {
            if(!m_validated) {
                foreach(var; m_context.variables) {
                    if(!var.isSet) throw new ExpressionError(format!"uninitialized variable (%s)"(var.name));
                }
                foreach(fn; m_context.functions) {
                    if(!fn.isSet) throw new ExpressionError(format!"uninitialized function (%s)"(fn.name));
                }
                m_validated = true;
            }
        }
    }
}

Expression!V compileExpression(R, V = float)(R source) {
    return Expression!V(source);
}

private:

struct Context(V) {
    Variable!V[] variables;
    Function!V[] functions;
    auto defineVariable(string identifier) {
        foreach(v; variables) if(v.name == identifier) return v;
        auto v = new Variable!V(identifier);
        variables ~= v;
        return v;
    }
    auto defineFunction(string identifier, Node!V[] args...) {
        foreach(f; functions) if(f.name == identifier && f.arity == args.length) return f;
        auto f = new Function!V(identifier, args);
        functions ~= f;
        return f;
    }
}

alias compileExpr = compileAdd;

Node!V compileAdd(R, V)(ref Source!R src, ref Context!V ctx) {
    auto arg = compileMul(src, ctx);
    skipWS(src);
    while(!src.empty) {
        switch(src.front) {
            case '+':
                src.popFront();
                arg = new Binary!(V, "+")(arg, compileMul(src, ctx));
                break;
            case '-':
                src.popFront();
                arg = new Binary!(V, "-")(arg, compileMul(src, ctx));
                break;
            default:
                return arg;
        }
        skipWS(src);
    }
    return arg;
}

Node!V compileMul(R, V)(ref Source!R src, ref Context!V ctx) {
    auto arg = compileValue(src, ctx);
    skipWS(src);
    while(!src.empty) {
        switch(src.front) {
            case '*':
                src.popFront();
                arg = new Binary!(V, "*")(arg, compileValue(src, ctx));
                break;
            case '/':
                src.popFront();
                arg = new Binary!(V, "/")(arg, compileValue(src, ctx));
                break;
            default:
                return arg;
        }
        skipWS(src);
    }
    return arg;
}

Node!V compileValue(R, V)(ref Source!R src, ref Context!V ctx) {
    skipWS(src);
    if(src.empty) throw new ExpressionError(src, "unexpected end of expression");
    if(isNumber(src.front)) {
        // literal
        return new Literal!V(parse!V(src));
    } else if(isAlpha(src.front)) {
        // identifier
        string identifier = {
            dchar[] result;
            while(!src.empty && isAlphaNum(src.front)) {
                result ~= src.front;
                src.popFront();
            }
            return result.toUTF8();
        }();

        skipWS(src);
        if(!src.empty && src.front == '(') {
            // function
            Node!V[] args;
            src.popFront();
            skipWS(src);
            while(!src.empty) {
                if(src.front == ')') {
                    src.popFront();
                    return ctx.defineFunction(identifier, args);
                }
                if(args.length > 0) {
                    if(src.front != ',') throw new ExpressionError(src, "comma expected");
                    src.popFront();
                }
                args ~= compileExpr(src, ctx);
                skipWS(src);
            }
            throw new ExpressionError(src, "unexpected end of expression");
        } else {
            // variable
            return ctx.defineVariable(identifier);
        }
    } else if(src.front == '-') {
        // unary minus
        src.popFront();
        return new Unary!(V, "-")(compileValue(src, ctx));
    } else if(src.front == '(') {
        src.popFront();
        auto expr = compileExpr(src, ctx);
        skipWS(src);
        if(src.front != ')') throw new ExpressionError(src, "closing parenthesis expected");
        src.popFront();
        return expr;
    } else {
        throw new ExpressionError(src, "value expected");
    }
}

void skipWS(R)(ref Source!R src) {
    while(!src.empty && isWhite(src.front)) src.popFront();
}

struct Source(R) {
    import std.array;
    R m_src;
    int m_pos = 0;

    bool empty() const { return m_src.empty; }
    dchar front() const { return m_src.front; }
    void popFront() { m_pos++; m_src.popFront(); }
    size_t pos() const { return m_pos; }
}

interface Node(V) {
    alias Node = .Node!V;
    V opCall() const;
    const(Node)[] children() const;
    string toString() const;
}

class Literal(V) : Node!V {
    private V m_value;
    this(V value) { m_value = value; }
    V opCall() const { return m_value; }
    const(Node)[] children() const { return null; }
    override string toString() const { return format!"Literal(%s)"(m_value); }
}

unittest {
    Node!float node = new Literal!float(1.2);
    assert(node().isClose(1.2));
    assert(node.children is null);
}

class Unary(V, string op) : Node!V {
    private Node m_arg;
    this(Node a) { m_arg = a; }
    V opCall() const { return mixin(op ~ `m_arg()`); }
    const(Node)[] children() const { return [m_arg]; }
    override string toString() const { return "Unary(" ~ op ~ ")"; }
}

class Binary(V, string op) : Node!V {
    private Node[2] m_args;
    this(Node a, Node b) { m_args = [a, b]; }
    V opCall() const {
        V result = mixin(`m_args[0]() ` ~ op ~ ` m_args[1]()`);
        static if(op == "/") {
            if(result.isInfinity) throw new ExpressionError("division by zero");
        }
        return result;
    }
    const(Node)[] children() const { return m_args; }
    override string toString() const { return "Binary(" ~ op ~ ")"; }
}

class Variable(V) : Node!V {
    private {
        string m_name;
        Nullable!V m_value;
    }
    this(string name) { m_name = name; }
    V opCall() const { return m_value.get(); }
    string name() const { return m_name; }
    void set(V value) { m_value = value; }
    bool isSet() const { return !m_value.isNull; }
    const(Node)[] children() const { return null; }
    override string toString() const { return format!"Variable(%s)"(m_name); }
}

class Function(V) : Node!V {
    private {
        string m_name;
        V delegate() m_fn;
        Node[] m_args;
    }

    this(string name, Node[] args...) {
        m_name = name;
        m_args = args;
    }

    size_t arity() const { return m_args.length; }

    void set(F)(F fn) {
        enum fna = .arity!fn;
        assert(fna == m_args.length, "wrong arity");
        enum params = iota(0, fna).map!(i => format!"m_args[%s]()"(i)).join(", ");
        m_fn = { return mixin("fn(" ~ params ~ ")"); };
    }

    V opCall() const { return m_fn(); }
    string name() const { return m_name; }
    bool isSet() const { return m_fn !is null; }
    const(Node)[] children() const { return m_args; }
    override string toString() const { return format!"Function(%s)"(arity); }
}

version(unittest) import std.math : isClose, sqrt;
unittest {

    float result = {
        float a = 3.2;
        float b = 9.6;
        auto sqrt = (float x) => .sqrt(x);
        auto dist1 = (float dx) => .sqrt(dx*dx + dx*dx);
        auto dist2 = (float dx, float dy) => .sqrt(dx*dx + dy*dy);
        return dist2(sqrt(a), b) * (a + 5.8)/3 + b/a - (235.6 + 3 * b) / -2.5 + dist1(a);
    }();

    auto expr(string source) {
        auto e = compileExpression(source);
        e["a"] = 3.2;
        e.b = 9.6;
        e["sqrt"] = (float x) => .sqrt(x);
        e.dist = (float dx) => .sqrt(dx*dx + dx*dx);
        e.dist = (float dx, float dy) => .sqrt(dx*dx + dy*dy);
        return e();
    }

    assert(expr("dist(sqrt(a),b)*(a+5.8)/3+b/a-(235.6+3*b)/-2.5+dist(a)").isClose(result));
    assert(expr("dist ( sqrt ( a ) , b ) * ( a + 5.8 ) / 3 + b / a - ( 235.6 + 3 * b) / - 2.5 + dist ( a )").isClose(result));
}
