const std = @import("std");
const testing = std.testing;

const Lexer = @import("../lexer/lexer.zig");
const object = @import("../object/object.zig");
const boolean = @import("../object/boolean.zig");
const ast = @import("../parser/ast.zig");
const Parser = @import("../parser/parser.zig");

pub fn eval(node: ast.Node) !object.Object {
    return switch (node) {
        .statement => |stmt| switch (stmt) {
            .program => |prog| {
                var result = object.Object{ .null = {} };
                for (prog.statements) |statement| {
                    result = try eval(statement.node());
                }
                return result;
            },
            .expression => |expr_stmt| try eval(expr_stmt.expression.node()),
            else => {
                std.debug.print("Unknown statement type: {}\n", .{stmt});
                unreachable;
            },
        },
        .expression => |expr| switch (expr) {
            .integer_literal => |int_lit| .{ .integer = .{ .value = int_lit.value } },
            else => {
                std.debug.print("Unknown expression type: {}\n", .{expr});
                unreachable;
            },
            .boolean_literal => |bool_lit| .{ .boolean = if (bool_lit.value) boolean.True else boolean.False },
            .prefix => |prefix_expr| {
                const right = try eval(prefix_expr.right.node());
                return evalPrefixExpression(prefix_expr.operator, right);
            },
        },
    };
}

fn evalPrefixExpression(operator: ast.PrefixOperator, right: object.Object) !object.Object {
    return switch (operator) {
        .bang => evalBangOperatorExpression(right),
        .minus => evalMinusPrefixOperatorExpression(right),
    };
}

fn evalBangOperatorExpression(right: object.Object) !object.Object {
    return switch (right) {
        .boolean => |bool_obj| .{ .boolean = if (bool_obj.value) boolean.False else boolean.True },
        .integer => |int_obj| .{ .boolean = if (int_obj.value == 0) boolean.True else boolean.False },
        .null => .{ .boolean = boolean.True },
    };
}

fn evalMinusPrefixOperatorExpression(right: object.Object) !object.Object {
    return switch (right) {
        .integer => |int_obj| .{ .integer = .{ .value = -int_obj.value } },
        .boolean => |bool_obj| .{ .integer = .{ .value = if (bool_obj.value) -1 else 0 } },
        else => unreachable,
    };
}

fn testEval(input: []const u8) !object.Object {
    const allocator = testing.allocator;
    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    return try eval(program.*.node());
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
    };

    for (tests) |t| {
        const evaluated = try testEval(t.input);
        try testIntegerObject(evaluated, t.expected);
    }
}

test "boolean expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{ .input = "true;", .expected = true },
        .{ .input = "false;", .expected = false },
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
