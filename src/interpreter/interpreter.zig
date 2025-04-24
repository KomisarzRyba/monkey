const std = @import("std");
const testing = std.testing;

const Lexer = @import("../lexer/lexer.zig");
const Boolean = @import("../object/boolean.zig");
const Integer = @import("../object/integer.zig");
const object = @import("../object/object.zig");
const ast = @import("../parser/ast.zig");
const Parser = @import("../parser/parser.zig");

pub fn eval(node: ast.Node) anyerror!object.Object {
    return switch (node) {
        .statement => |stmt| switch (stmt) {
            .program => |prog| try evalProgram(prog),
            .expression => |expr_stmt| try eval(expr_stmt.expression.node()),
            .block => |block_stmt| try evalBlockStatement(block_stmt),
            .@"return" => |return_stmt| {
                var return_value = try eval(return_stmt.return_value.node());
                return object.Object{ .@"return" = &return_value };
            },
            else => {
                std.debug.print("Unknown statement type: {}\n", .{stmt});
                unreachable;
            },
        },
        .expression => |expr| switch (expr) {
            .integer_literal => |int_lit| .{ .integer = .{ .value = int_lit.value } },
            .boolean_literal => |bool_lit| .{ .boolean = if (bool_lit.value) &Boolean.True else &Boolean.False },
            .prefix => |prefix_expr| {
                const right = try eval(prefix_expr.right.node());
                return evalPrefixExpression(prefix_expr.operator, right);
            },
            .infix => |infix_expr| {
                const left = try eval(infix_expr.left.node());
                const right = try eval(infix_expr.right.node());
                return evalInfixExpression(infix_expr.operator, left, right);
            },
            .@"if" => |if_expr| try evalIfExpression(if_expr),
            else => {
                std.debug.print("Unknown expression type: {}\n", .{expr});
                unreachable;
            },
        },
    };
}

fn evalProgram(program: ast.Program) !object.Object {
    var result = object.Object{ .null = {} };
    for (program.statements) |statement| {
        result = try eval(statement.node());
        if (result == .@"return") {
            return result;
        }
    }
    return result;
}

fn evalBlockStatement(block: ast.BlockStatement) !object.Object {
    var result = object.Object{ .null = {} };
    for (block.statements) |statement| {
        result = try eval(statement.node());
        if (result == .@"return") {
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
    };
}

fn evalMinusPrefixOperatorExpression(right: object.Object) !object.Object {
    return switch (right) {
        .integer => |int_obj| .{ .integer = .{ .value = -int_obj.value } },
        .boolean => |bool_obj| .{ .integer = .{ .value = if (bool_obj.value) -1 else 0 } },
        else => unreachable,
    };
}

fn evalInfixExpression(operator: ast.InfixOperator, left: object.Object, right: object.Object) !object.Object {
    if (left == .integer and right == .integer) {
        return evalIntegerInfixExpression(operator, left.integer, right.integer);
    } else if (operator == .eq) {
        return .{ .boolean = if (left.eq(right)) &Boolean.True else &Boolean.False };
    } else if (operator == .not_eq) {
        return .{ .boolean = if (!left.eq(right)) &Boolean.True else &Boolean.False };
    } else unreachable;
}

fn evalIntegerInfixExpression(operator: ast.InfixOperator, left: Integer, right: Integer) !object.Object {
    return switch (operator) {
        .plus => .{ .integer = .{ .value = left.value + right.value } },
        .minus => .{ .integer = .{ .value = left.value - right.value } },
        .asterisk => .{ .integer = .{ .value = left.value * right.value } },
        .slash => .{ .integer = .{ .value = @divFloor(left.value, right.value) } },
        .lt => .{ .boolean = if (left.value < right.value) &Boolean.True else &Boolean.False },
        .gt => .{ .boolean = if (left.value > right.value) &Boolean.True else &Boolean.False },
        .eq => .{ .boolean = if (left.value == right.value) &Boolean.True else &Boolean.False },
        .not_eq => .{ .boolean = if (left.value != right.value) &Boolean.True else &Boolean.False },
    };
}

fn evalIfExpression(if_expr: ast.IfExpression) !object.Object {
    const condition = try eval(if_expr.condition.*.node());
    if (condition.truthy()) {
        return try eval(if_expr.consequence.*.node());
    } else if (if_expr.alternative) |alt| {
        return try eval(alt.*.node());
    } else {
        return object.Object{ .null = {} };
    }
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
