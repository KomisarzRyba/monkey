const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
const ArenaAllocator = std.heap.ArenaAllocator;

const Lexer = @import("../lexer/lexer.zig");
const ast = @import("../parser/ast.zig");
const Parser = @import("../parser/parser.zig");
const Environment = @import("environment.zig");
const Boolean = @import("object/boolean.zig");
const Error = @import("object/error.zig");
const Integer = @import("object/integer.zig");
const object = @import("object/object.zig");
const Function = @import("object/function.zig");

pub fn eval(node: ast.Node, env: *Environment) anyerror!object.Object {
    return switch (node) {
        .statement => |stmt| switch (stmt) {
            .program => |prog| try evalProgram(prog, env),
            .expression => |expr_stmt| try eval(expr_stmt.expression.node(), env),
            .block => |block_stmt| try evalBlockStatement(block_stmt, env),
            .@"return" => |return_stmt| {
                var return_value = try eval(return_stmt.return_value.node(), env);
                if (return_value == .@"error") return return_value;
                return object.Object{ .@"return" = &return_value };
            },
            .let => |let_stmt| {
                const value = try eval(let_stmt.value.node(), env);
                if (value == .@"error") return value;
                try env.set(let_stmt.name.value, value);
                return value;
            },
        },
        .expression => |expr| switch (expr) {
            .integer_literal => |int_lit| .{ .integer = .{ .value = int_lit.value } },
            .string_literal => |str_lit| .{ .string = .{ .value = str_lit.value } },
            .boolean_literal => |bool_lit| .{ .boolean = if (bool_lit.value) &Boolean.True else &Boolean.False },
            .prefix => |prefix_expr| {
                const right = try eval(prefix_expr.right.node(), env);
                if (right == .@"error") return right;
                return try evalPrefixExpression(prefix_expr.operator, right);
            },
            .infix => |infix_expr| {
                const left = try eval(infix_expr.left.node(), env);
                if (left == .@"error") return left;
                const right = try eval(infix_expr.right.node(), env);
                if (right == .@"error") return right;
                return try evalInfixExpression(infix_expr.operator, left, right);
            },
            .@"if" => |if_expr| try evalIfExpression(if_expr, env),
            .identifier => |ident| try evalIdentifier(ident, env),
            .function_literal => |fn_lit| .{
                .function = try .init(
                    fn_lit.parameters,
                    fn_lit.body,
                    env,
                ),
            },
            .call => |call_expr| {
                const function = try eval(call_expr.function.*.node(), env);
                if (function == .@"error") return function;
                const args = try evalExpressions(call_expr.arguments, env);
                if (args.len == 1 and args[0] == .@"error") return args[0];
                const res = try applyFunction(function.function, args);
                return res;
            },
        },
    };
}

fn evalProgram(program: ast.Program, env: *Environment) !object.Object {
    var result = object.Object{ .null = {} };
    for (program.statements) |statement| {
        result = try eval(statement.node(), env);
        if (result == .@"return" or result == .@"error") {
            return result;
        }
    }
    return result;
}

fn evalBlockStatement(block: ast.BlockStatement, env: *Environment) !object.Object {
    var result = object.Object{ .null = {} };
    for (block.statements) |statement| {
        result = try eval(statement.node(), env);
        if (result == .@"return" or result == .@"error") {
            return result;
        }
    }
    return result;
}

fn evalPrefixExpression(operator: ast.PrefixOperator, right: object.Object) !object.Object {
    return switch (operator) {
        .bang => evalBangOperatorExpression(right),
        .minus => evalMinusPrefixOperatorExpression(right),
    };
}

fn evalBangOperatorExpression(right: object.Object) !object.Object {
    return switch (right) {
        .boolean => |bool_obj| .{ .boolean = if (bool_obj.value) &Boolean.False else &Boolean.True },
        .integer => |int_obj| .{ .boolean = if (int_obj.value == 0) &Boolean.True else &Boolean.False },
        .null => .{ .boolean = &Boolean.True },
        .@"return" => |return_obj| try evalBangOperatorExpression(return_obj.*),
        else => unreachable,
    };
}

fn evalMinusPrefixOperatorExpression(right: object.Object) !object.Object {
    return switch (right) {
        .integer => |int_obj| .{ .integer = .{ .value = -int_obj.value } },
        else => .{ .@"error" = Error.new("unknown operator: -{s}", .{@tagName(right)}) },
    };
}

fn evalInfixExpression(operator: ast.InfixOperator, left: object.Object, right: object.Object) !object.Object {
    if (left == .integer and right == .integer) {
        return try evalIntegerInfixExpression(operator, left.integer, right.integer);
    } else if (operator == .eq) {
        return .{ .boolean = if (left.eq(right)) &Boolean.True else &Boolean.False };
    } else if (operator == .not_eq) {
        return .{ .boolean = if (!left.eq(right)) &Boolean.True else &Boolean.False };
    } else {
        return object.Object{ .@"error" = Error.new("unknown operator: {s} {s} {s}", .{ @tagName(left), operator.toString(), @tagName(right) }) };
    }
}

fn evalIntegerInfixExpression(operator: ast.InfixOperator, left: Integer, right: Integer) !object.Object {
    return switch (operator) {
        .plus => .{ .integer = .{ .value = left.value + right.value } },
        .minus => .{ .integer = .{ .value = left.value - right.value } },
        .asterisk => .{ .integer = .{ .value = left.value * right.value } },
        .slash => .{ .integer = .{ .value = @divFloor(left.value, right.value) } },
        .lt => .{ .boolean = if (left.value < right.value) &Boolean.True else &Boolean.False },
        .lte => .{ .boolean = if (left.value <= right.value) &Boolean.True else &Boolean.False },
        .gt => .{ .boolean = if (left.value > right.value) &Boolean.True else &Boolean.False },
        .gte => .{ .boolean = if (left.value >= right.value) &Boolean.True else &Boolean.False },
        .eq => .{ .boolean = if (left.value == right.value) &Boolean.True else &Boolean.False },
        .not_eq => .{ .boolean = if (left.value != right.value) &Boolean.True else &Boolean.False },
    };
}

fn evalIfExpression(if_expr: ast.IfExpression, env: *Environment) !object.Object {
    const condition = try eval(if_expr.condition.*.node(), env);
    if (condition == .@"error") return condition;
    if (condition.truthy()) {
        return try eval(if_expr.consequence.*.node(), env);
    } else if (if_expr.alternative) |alt| {
        return try eval(alt.*.node(), env);
    } else {
        return object.Object{ .null = {} };
    }
}

fn evalIdentifier(ident: ast.Identifier, env: *Environment) !object.Object {
    const value = env.get(ident.value);
    if (value) |v| {
        return v;
    }
    return .{ .@"error" = Error.new("identifier not found: {s}", .{ident.value}) };
}

fn evalExpressions(expressions: []ast.Expression, env: *Environment) ![]object.Object {
    var result = std.ArrayList(object.Object).init(env.allocator);
    defer result.deinit();

    for (expressions) |expr| {
        const evaluated = try eval(expr.node(), env);
        if (evaluated == .@"error") return result.toOwnedSlice();
        try result.append(evaluated);
    }

    return result.toOwnedSlice();
}

fn applyFunction(func: Function, args: []object.Object) !object.Object {
    const extended_env = try extendFunctionEnv(func, args);
    const evaluated = try eval(func.body.node(), extended_env);
    return unwrapReturnValue(evaluated);
}

fn extendFunctionEnv(func: Function, args: []object.Object) !*Environment {
    const allocator = func.env.allocator;

    const extended_env_ptr = try allocator.create(Environment);
    extended_env_ptr.* = Environment.init_enclosed(allocator, func.env);

    for (func.params, 0..) |param, i| {
        try extended_env_ptr.set(param.value, args[i]);
    }
    return extended_env_ptr;
}

fn unwrapReturnValue(obj: object.Object) object.Object {
    if (obj == .@"return") {
        return obj.@"return".*;
    }
    return obj;
}

fn testEval(input: []const u8) !object.Object {
    const allocator = testing.allocator;

    var env_arena = ArenaAllocator.init(allocator);
    defer env_arena.deinit();

    const env_alloc = env_arena.allocator();
    var env = Environment.init(env_alloc);
    // defer env.deinit();

    var ast_arena = ArenaAllocator.init(allocator);
    defer ast_arena.deinit();

    var lexer = Lexer.init(input);

    const parser_alloc = ast_arena.allocator();
    var parser = Parser.init(parser_alloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    return try eval(program.*.node(), &env);
}

fn testEvalUnmanaged(allocator: Allocator, env: *Environment, input: []const u8) !object.Object {
    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    return try eval(program.*.node(), env);
}

fn testIntegerObject(obj: object.Object, expected: i64) !void {
    try testing.expectEqual(expected, obj.integer.value);
}

fn testBooleanObject(obj: object.Object, expected: bool) !void {
    try testing.expectEqual(expected, obj.boolean.value);
}

test "integer expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5;", .expected = 5 },
        .{ .input = "10;", .expected = 10 },
        .{ .input = "-5;", .expected = -5 },
        .{ .input = "-10;", .expected = -10 },
        .{ .input = "5 + 5 + 5 + 5 - 10;", .expected = 10 },
        .{ .input = "2 * 2 * 2 * 2 * 2;", .expected = 32 },
        .{ .input = "-50 + 100 + -50;", .expected = 0 },
        .{ .input = "5 * 2 + 10;", .expected = 20 },
        .{ .input = "5 + 2 * 10;", .expected = 25 },
        .{ .input = "20 + 2 * -10;", .expected = 0 },
        .{ .input = "50 / 2 * 2 + 10;", .expected = 60 },
        .{ .input = "2 * (5 + 10);", .expected = 30 },
        .{ .input = "3 * 3 * 3 + 10;", .expected = 37 },
        .{ .input = "3 * (3 * 3) + 10;", .expected = 37 },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10;", .expected = 50 },
    };

    for (tests) |t| {
        const evaluated = try testEval(t.input);
        try testIntegerObject(evaluated, t.expected);
    }
}

test "string expression" {
    const input = "\"Hello World!\";";
    const evaluated = try testEval(input);
    try testing.expectEqualStrings("Hello World!", evaluated.string.value);
}

test "boolean expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "true;", .expected = true },
        .{ .input = "false;", .expected = false },
        .{ .input = "1 < 2;", .expected = true },
        .{ .input = "1 > 2;", .expected = false },
        .{ .input = "1 < 1;", .expected = false },
        .{ .input = "1 > 1;", .expected = false },
        .{ .input = "1 == 1;", .expected = true },
        .{ .input = "1 != 1;", .expected = false },
        .{ .input = "1 == 2;", .expected = false },
        .{ .input = "1 != 2;", .expected = true },
        .{ .input = "true == true;", .expected = true },
        .{ .input = "false == false;", .expected = true },
        .{ .input = "true != false;", .expected = true },
        .{ .input = "false != true;", .expected = true },
        .{ .input = "true == false;", .expected = false },
        .{ .input = "false == true;", .expected = false },
        .{ .input = "(1 < 2) == true;", .expected = true },
        .{ .input = "(1 < 2) == false;", .expected = false },
        .{ .input = "(1 > 2) == true;", .expected = false },
        .{ .input = "(1 > 2) == false;", .expected = true },
    };

    for (tests) |t| {
        const evaluated = try testEval(t.input);
        try testBooleanObject(evaluated, t.expected);
    }
}

test "bang operators" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "!true;", .expected = false },
        .{ .input = "!false;", .expected = true },
        .{ .input = "!5;", .expected = false },
        .{ .input = "!!true;", .expected = true },
        .{ .input = "!!false;", .expected = false },
        .{ .input = "!!5;", .expected = true },
    };

    for (tests) |t| {
        const evaluated = try testEval(t.input);
        try testBooleanObject(evaluated, t.expected);
    }
}

test "if expressions" {
    const Result = union(enum) {
        integer: i64,
        null,
    };
    const tests = [_]struct {
        input: []const u8,
        expected: Result,
    }{
        .{ .input = "if (true) { 10 };", .expected = .{ .integer = 10 } },
        .{ .input = "if (false) { 10 };", .expected = .{ .null = {} } },
        .{ .input = "if (1) { 10 };", .expected = .{ .integer = 10 } },
        .{ .input = "if (1 < 2) { 10 };", .expected = .{ .integer = 10 } },
        .{ .input = "if (1 > 2) { 10 };", .expected = .{ .null = {} } },
        .{ .input = "if (1 < 2) { 10 } else { 20 };", .expected = .{ .integer = 10 } },
        .{ .input = "if (1 > 2) { 10 } else { 20 };", .expected = .{ .integer = 20 } },
    };

    for (tests) |t| {
        const evaluated = try testEval(t.input);
        switch (evaluated) {
            .integer => {
                try testIntegerObject(evaluated, t.expected.integer);
            },
            .null => {
                try testing.expectEqual(t.expected.null, evaluated.null);
            },
            else => unreachable,
        }
    }
}

test "return statements" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "return 10;", .expected = 10 },
        .{ .input = "return 10; 9;", .expected = 10 },
        .{ .input = "return 2 * 5; 9;", .expected = 10 },
        .{ .input = "9; return 2 * 5; 9;", .expected = 10 },
        .{ .input = "if (10 > 1) { if (10 > 1) { return 10; } return 1; }", .expected = 10 },
    };

    for (tests) |t| {
        const evaluated = try testEval(t.input);
        try testIntegerObject(evaluated.@"return".*, t.expected);
    }
}

test "error handling" {
    const tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{ .input = "5 > true;", .expected = "unknown operator: integer > boolean" },
        .{ .input = "5 >= true; 5;", .expected = "unknown operator: integer >= boolean" },
        .{ .input = "-true;", .expected = "unknown operator: -boolean" },
        .{ .input = "true + false;", .expected = "unknown operator: boolean + boolean" },
        .{ .input = "5; true + false; 5;", .expected = "unknown operator: boolean + boolean" },
        .{ .input = "if (10 > 1) { true + false; }", .expected = "unknown operator: boolean + boolean" },
        .{ .input = "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }", .expected = "unknown operator: boolean + boolean" },
        .{ .input = "foobar;", .expected = "identifier not found: foobar" },
    };

    for (tests) |t| {
        const evaluated = try testEval(t.input);
        std.debug.print("evaluated: {s}\n", .{evaluated.inspect()});
        try testing.expectEqualStrings(t.expected, evaluated.@"error".message);
    }
}

test "let statements" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "let x = 5; x;", .expected = 5 },
        .{ .input = "let x = 5 * 5; x;", .expected = 25 },
        .{ .input = "let x = 5; let y = x + 10; y;", .expected = 15 },
        .{ .input = "let x = 5; let y = x + 10; let z = y + 10; z;", .expected = 25 },
    };

    for (tests) |t| {
        const evaluated = try testEval(t.input);
        try testIntegerObject(evaluated, t.expected);
    }
}

test "function" {
    const input = "fn(x) { x + 2; };";

    const allocator = testing.allocator;

    var env_arena = ArenaAllocator.init(allocator);
    defer env_arena.deinit();

    const env_alloc = env_arena.allocator();
    var environment = Environment.init(env_alloc);
    defer environment.deinit();

    var ast_arena = ArenaAllocator.init(allocator);
    defer ast_arena.deinit();

    const evaluated = try testEvalUnmanaged(ast_arena.allocator(), &environment, input);

    try testing.expectEqual(1, evaluated.function.params.len);
    try testing.expectEqualStrings("x", evaluated.function.params[0].value);
    try testing.expectEqualStrings("(x + 2)", evaluated.function.body.toString());
}

test "function application" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "let identity = fn(x) { x; }; identity(5);", .expected = 5 },
        .{ .input = "let identity = fn(x) { return x; }; identity(5);", .expected = 5 },
        .{ .input = "let double = fn(x) { x * 2; }; double(5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5, 5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5, add(5, 5));", .expected = 15 },
    };

    for (tests) |t| {
        var env_arena = ArenaAllocator.init(testing.allocator);
        defer env_arena.deinit();

        const env_alloc = env_arena.allocator();
        var environment = Environment.init(env_alloc);
        defer environment.deinit();

        var ast_arena = ArenaAllocator.init(testing.allocator);
        defer ast_arena.deinit();

        const evaluated = try testEvalUnmanaged(ast_arena.allocator(), &environment, t.input);
        try testIntegerObject(evaluated, t.expected);
    }
}

test "closures" {
    const input =
        \\let newAdder = fn(x) {
        \\    fn(y) { x + y; };
        \\};
        \\let addTwo = newAdder(2);
        \\addTwo(2);
    ;

    var env_arena = ArenaAllocator.init(testing.allocator);
    defer env_arena.deinit();

    const env_alloc = env_arena.allocator();
    var environment = Environment.init(env_alloc);
    defer environment.deinit();

    var ast_arena = ArenaAllocator.init(testing.allocator);
    defer ast_arena.deinit();

    const evaluated = try testEvalUnmanaged(ast_arena.allocator(), &environment, input);
    try testIntegerObject(evaluated, 4);
}
