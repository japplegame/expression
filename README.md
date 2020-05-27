#### Simple expression evaluator

Supports +, -, *, /, (), variables and functions.
```d
import std.stdio;
import std.math;

import expression;

void main() {
    auto expr = compileExpression("base + delta/(0.5 * begin - end) + sqrt(min(base / 10, delta))");

    static assert(is(typeof(expr) == Expression!float));

    // set variables and functions as properties
    expr.base = 1001.3;
    expr.delta = 535.4;
    expr.min = (float x, float y) => x < y ? x : y;

    // or as indexes
    expr["begin"] = 34;
    expr["end"] = 12;
    expr["sqrt"] = (float x) => sqrt(x);

    // evaluate
    writeln("result: ", expr()); // 1118.39

    // functions with different arities may have the same name
    expr = compileExpression("dist(x, y) + dist(x)");
    expr.dist = (float x, float y) => sqrt(x*x + y*y);
    expr.dist = (float x) => sqrt((x + 2) * (x + 2) + (x + 9) * (x + 9));
    expr.x = 3;
    expr.y = 4;

    writeln("result: ", expr()); // 18
}
```
