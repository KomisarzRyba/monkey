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
            "let {s} = {s};",
            .{ self.name.toString(), self.value.toString() },
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
            .{self.return_value.toString()},
        ) catch unreachable;
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: Expression,

    pub fn toString(self: ExpressionStatement) []const u8 {
        return std.fmt.allocPrint(
            std.heap.page_allocator,
            "{s}",
            .{self.expression.toString()},
        ) catch unreachable;
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    prefix: Prefix,
    infix: Infix,
    boolean_literal: BooleanLiteral,

    pub fn toString(self: Expression) []const u8 {
        return switch (self) {
            .identifier => |e| e.toString(),
            .integer_literal => |e| e.toString(),
            .prefix => |e| e.toString(),
            .infix => |e| e.toString(),
            .boolean_literal => |e| e.toString(),
        };
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn toString(self: Identifier) []const u8 {
        return self.value;
    }
};

pub const IntegerLiteral = struct {
    token: Token,
    value: i64,

    pub fn toString(self: IntegerLiteral) []const u8 {
        return std.fmt.allocPrint(
            std.heap.page_allocator,
            "{d}",
            .{self.value},
        ) catch unreachable;
    }
};

pub const Prefix = struct {
    token: Token,
    operator: PrefixOperator,
    right: *Expression,

    pub fn toString(self: Prefix) []const u8 {
        return std.fmt.allocPrint(
            std.heap.page_allocator,
            "({s}{s})",
            .{ self.operator.toString(), self.right.toString() },
        ) catch unreachable;
    }
};

pub const PrefixOperator = enum {
    minus,
    bang,

    pub fn toString(self: PrefixOperator) []const u8 {
        return switch (self) {
            .minus => "-",
            .bang => "!",
        };
    }

    pub fn fromTokenType(token_type: Token.Type) PrefixOperator {
        return switch (token_type) {
            .minus => .minus,
            .bang => .bang,
            else => unreachable,
        };
    }
};

pub const Infix = struct {
    token: Token,
    left: *Expression,
    operator: InfixOperator,
    right: *Expression,

    pub fn toString(self: Infix) []const u8 {
        return std.fmt.allocPrint(
            std.heap.page_allocator,
            "({s} {s} {s})",
            .{ self.left.toString(), self.operator.toString(), self.right.toString() },
        ) catch unreachable;
    }
};

pub const InfixOperator = enum {
    plus,
    minus,
    asterisk,
    slash,
    lt,
    gt,
    eq,
    not_eq,

    pub fn toString(self: InfixOperator) []const u8 {
        return switch (self) {
            .plus => "+",
            .minus => "-",
            .asterisk => "*",
            .slash => "/",
            .lt => "<",
            .gt => ">",
            .eq => "==",
            .not_eq => "!=",
        };
    }

    pub fn fromTokenType(token_type: Token.Type) InfixOperator {
        return switch (token_type) {
            .plus => .plus,
            .minus => .minus,
            .asterisk => .asterisk,
            .slash => .slash,
            .lt => .lt,
            .gt => .gt,
            .eq => .eq,
            .not_eq => .not_eq,
            else => unreachable,
        };
    }
};

pub const BooleanLiteral = struct {
    token: Token,
    value: bool,

    pub fn toString(self: BooleanLiteral) []const u8 {
        return if (self.value) "true" else "false";
    }
};

pub const Program = struct {
    statements: []Statement,

    pub fn toString(self: Program) []const u8 {
        var program_str = std.ArrayList(u8).init(std.heap.page_allocator);
        defer program_str.deinit();

        for (self.statements) |stmt| {
            program_str.appendSlice(stmt.toString()) catch unreachable;
        }

        return program_str.toOwnedSlice() catch unreachable;
    }
};
