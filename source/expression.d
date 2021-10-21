module expression;

import std.algorithm : map, find;
import std.conv : parse, to;
import std.format : formattedWrite;
import std.math : isInfinity;
import std.meta : AliasSeq;
import std.range : ElementType, iota, isInputRange, repeat;
import std.string : format, join;
import std.traits : arity, isSomeChar, isFloatingPoint;
import std.typecons : Nullable;
import std.uni : isAlpha, isAlphaNum, isNumber, isWhite;
import std.utf : toUTF8;

class ExpressionError : Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }

    this(S)(ref const S source, string msg, string file = __FILE__, size_t line = __LINE__) {
        super("%s (%s)".format(msg, source.pos + 1), file, line);
    }
}

struct Expression(V) {
    @disable this();

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
        auto p = name in m_context.variables;
        if (p is null)
            throw new ExpressionError("undefined variable: " ~ name);
        (*p).set(value);
    }

    void opIndexAssign(F)(F fn, string name) {
        // check function type;
        mixin("alias VS = AliasSeq!(" ~ "V.init".repeat(arity!F).join(",") ~ ");");
        V v;
        static assert(__traits(compiles, v = fn(VS)), "invalid function type:" ~ F.stringof);

        auto p = (name ~= arity!F.to!string) in m_context.functions;
        if (p is null)
            throw new ExpressionError(format!"undefined function (%s/%s)"(name, arity!F));
        (*p).set(fn);
    }

    template opDispatch(string name) {
        void opDispatch(V)(V value) { this[name] = value; }
    }

    auto variables() { return m_context.variables.keys; }
    auto functions() { return m_context.functions.keys; }

    private:
        Node!V m_root;
        Context!V m_context;
        bool m_validated = false;

        void validate() {
            if(!m_validated) {
                foreach(var; m_context.variables.values) {
                    if(!var.isSet) throw new ExpressionError("uninitialized variable: " ~ var.name);
                }
                foreach(fn; m_context.functions.values) {
                    if(!fn.isSet) throw new ExpressionError("uninitialized function: " ~ fn.name);
                }
                m_validated = true;
            }
        }
}

Expression!V compileExpression(V = float, R)(R source) {
    return Expression!V(source);
}

private:

struct Context(V) {
    Variable!V[string] variables;
    Function!V[string] functions;
    auto defineVariable(string identifier) {
        auto p = identifier in variables;
        if(p !is null)
            return *p;
        return variables[identifier] = new Variable!V(identifier);
    }
    auto defineFunction(string identifier, Node!V[] args...) {
        identifier ~= args.length.to!string;
        auto p = identifier in functions;
        if(p !is null)
            return *p;
        return functions[identifier] = new Function!V(identifier, args);
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
    }
    if(isAlpha(src.front)) {
        // identifier
        dchar[] result;
        while(!src.empty && isAlphaNum(src.front)) {
            result ~= src.front;
            src.popFront();
        }
        string identifier = result.toUTF8();

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
                if(args.length) {
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
    }
    if(src.front == '-') {
        // unary minus
        src.popFront();
        return new Unary!(V, "-")(compileValue(src, ctx));
    }
    if(src.front == '(') {
        src.popFront();
        auto expr = compileExpr(src, ctx);
        skipWS(src);
        if(src.front != ')') throw new ExpressionError(src, "closing parenthesis expected");
        src.popFront();
        return expr;
    }
    throw new ExpressionError(src, "value expected");
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
    protected V m_val;
    this(V value) { m_val = value; }
    V opCall() const { return m_val; }
    const(Node)[] children() const { return null; }
    override string toString() const { return "Literal(" ~ m_val.to!string ~ ")"; }
}

unittest {
    Node!float node = new Literal!float(1.2);
    assert(node().isClose(1.2));
    assert(node.children is null);
}

class Unary(V, string op) : Node!V {
    protected Node m_arg;
    this(Node a) { m_arg = a; }
    V opCall() const { return mixin(op ~ `m_arg()`); }
    const(Node)[] children() const { return [m_arg]; }
    override string toString() const { return "Unary(" ~ op ~ ")"; }
}

class Binary(V, string op) : Node!V {
    protected Node[2] m_args;
    this(Node a, Node b) { m_args = [a, b]; }
    V opCall() const {
        V result = mixin(`m_args[0]() ` ~ op ~ ` m_args[1]()`);
        version(AllowDivisionBy0) {} else {
            static if(isFloatingPoint!V && op == "/") {
                if(result.isInfinity) throw new ExpressionError("division by zero");
            }
        }
        return result;
    }
    const(Node)[] children() const { return m_args; }
    override string toString() const { return "Binary(" ~ op ~ ")"; }
}

class Variable(V) : Node!V {
    protected {
        string m_name;
        Nullable!V m_value;
    }
    this(string name) { m_name = name; }
    V opCall() const { return m_value.get(); }
    string name() const { return m_name; }
    void set(V value) { m_value = value; }
    bool isSet() const { return !m_value.isNull; }
    const(Node)[] children() const { return null; }
    override string toString() const { return "Variable(" ~ m_name ~ ")"; }
}

class Function(V) : Node!V {
    protected {
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
        enum fna = .arity!F;
        assert(fna == m_args.length, format!"%s wrong arity: %s/%s"(m_name, fna, m_args.length));
        enum params = iota(0, fna).map!(i => format!"m_args[%s]()"(i)).join(",");
        m_fn = () => mixin("fn(" ~ params ~ ")");
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

unittest {

    int result = {
        int a = 3;
        int b = 9;
        auto sqr = (int x) => x*x;
        return ((a - 3) * b + 1) * (a + 5)/3 + b/a - (235 + 3 * b) / -2 + sqr(a);
    }();

    int expr(string source) {
        auto e = compileExpression!int(source);
        e["a"] = 3;
        e.b = 9;
        e["sqr"] = (int x) => x*x;
        return e();
    }

    assert(expr("((a - 3) * b + 1) * (a + 5)/3 + b/a - (235 + 3 * b) / -2 + sqr(a)") == result);
}