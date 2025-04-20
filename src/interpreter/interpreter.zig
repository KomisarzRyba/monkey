const std = @import("std");
const testing = std.testing;

const Lexer = @import("../lexer/lexer.zig");
const object = @import("../object/object.zig");
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
        },
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

test "integer expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5;", .expected = 5 },
        .{ .input = "10;", .expected = 10 },
    };

    for (tests) |t| {
        const evaluated = try testEval(t.input);
        try testIntegerObject(evaluated, t.expected);
    }
}
