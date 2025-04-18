const std = @import("std");

const Token = @import("../lexer/token.zig");

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,

    pub fn toString(self: Node) []const u8 {
        return switch (self) {
            .statement => |s| s.toString(),
            .expression => |e| e.toString(),
        };
    }
};

pub const Statement = union(enum) {
    let: LetStatement,
    @"return": ReturnStatement,
    expression: ExpressionStatement,

    pub fn toString(self: Statement) []const u8 {
        return switch (self) {
            .let => |s| s.toString(),
            .@"return" => |s| s.toString(),
            .expression => |s| s.toString(),
        };
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
    value: Expression,

    pub fn toString(self: LetStatement) []const u8 {
        return std.fmt.allocPrint(
            std.heap.page_allocator,
            "{s} = {s};",
            .{ self.name.toString(), self.value },
        ) catch unreachable;
    }
};

pub const ReturnStatement = struct {
    token: Token,
    return_value: Expression,

    pub fn toString(self: ReturnStatement) []const u8 {
        return std.fmt.allocPrint(
            std.heap.page_allocator,
            "return {s};",
            .{self.return_value},
        ) catch unreachable;
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: Expression,

    pub fn toString(self: ExpressionStatement) []const u8 {
        return std.fmt.allocPrint(
            std.heap.page_allocator,
            "{s};",
            .{self.expression.toString()},
        ) catch unreachable;
    }
};

pub const Expression = union(enum) {
    pub fn toString(self: Expression) []const u8 {
        _ = self;
        return "";
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn toString(self: Identifier) []const u8 {
        return self.value;
    }
};

pub const Program = struct {
    statements: []Statement,
};
