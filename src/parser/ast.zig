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
    program: Program,
    let: LetStatement,
    @"return": ReturnStatement,
    expression: ExpressionStatement,
    block: BlockStatement,

    pub fn toString(self: Statement) []const u8 {
        return switch (self) {
            .program => |p| p.toString(),
            .let => |s| s.toString(),
            .@"return" => |s| s.toString(),
            .expression => |s| s.toString(),
            .block => |s| s.toString(),
        };
    }

    pub fn node(self: Statement) Node {
        return Node{ .statement = self };
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

pub const BlockStatement = struct {
    token: Token,
    statements: []Statement,

    pub fn toString(self: BlockStatement) []const u8 {
        var block_str = std.ArrayList(u8).init(std.heap.page_allocator);
        defer block_str.deinit();

        for (self.statements) |stmt| {
            block_str.appendSlice(stmt.toString()) catch unreachable;
        }

        return block_str.toOwnedSlice() catch unreachable;
    }

    pub fn node(self: BlockStatement) Node {
        return Node{ .statement = .{ .block = self } };
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteralExpression,
    prefix: PrefixExpression,
    infix: InfixExpression,
    boolean_literal: BooleanLiteralExpression,
    @"if": IfExpression,
    function_literal: FunctionLiteralExpression,
    call: CallExpression,

    pub fn toString(self: Expression) []const u8 {
        return switch (self) {
            .identifier => |e| e.toString(),
            .integer_literal => |e| e.toString(),
            .prefix => |e| e.toString(),
            .infix => |e| e.toString(),
            .boolean_literal => |e| e.toString(),
            .@"if" => |e| e.toString(),
            .function_literal => |e| e.toString(),
            .call => |e| e.toString(),
        };
    }

    pub fn node(self: Expression) Node {
        return Node{ .expression = self };
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn toString(self: Identifier) []const u8 {
        return self.value;
    }
};

pub const IntegerLiteralExpression = struct {
    token: Token,
    value: i64,

    pub fn toString(self: IntegerLiteralExpression) []const u8 {
        return std.fmt.allocPrint(
            std.heap.page_allocator,
            "{d}",
            .{self.value},
        ) catch unreachable;
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: PrefixOperator,
    right: *Expression,

    pub fn toString(self: PrefixExpression) []const u8 {
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

pub const InfixExpression = struct {
    token: Token,
    left: *Expression,
    operator: InfixOperator,
    right: *Expression,

    pub fn toString(self: InfixExpression) []const u8 {
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
    lte,
    gt,
    gte,
    eq,
    not_eq,

    pub fn toString(self: InfixOperator) []const u8 {
        return switch (self) {
            .plus => "+",
            .minus => "-",
            .asterisk => "*",
            .slash => "/",
            .lt => "<",
            .lte => "<=",
            .gt => ">",
            .gte => ">=",
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
            .eq => .eq,
            .not_eq => .not_eq,
            .lt => .lt,
            .lte => .lte,
            .gte => .gte,
            .gt => .gt,
            else => unreachable,
        };
    }
};

pub const BooleanLiteralExpression = struct {
    token: Token,
    value: bool,

    pub fn toString(self: BooleanLiteralExpression) []const u8 {
        return if (self.value) "true" else "false";
    }
};

pub const IfExpression = struct {
    token: Token,
    condition: *Expression,
    consequence: *BlockStatement,
    alternative: ?*BlockStatement,

    pub fn toString(self: IfExpression) []const u8 {
        var if_str = std.ArrayList(u8).init(std.heap.page_allocator);
        defer if_str.deinit();

        if_str.appendSlice("if (") catch unreachable;
        if_str.appendSlice(self.condition.toString()) catch unreachable;
        if_str.appendSlice(") ") catch unreachable;
        if_str.appendSlice(self.consequence.toString()) catch unreachable;

        if (self.alternative) |alt| {
            if_str.appendSlice(" else ") catch unreachable;
            if_str.appendSlice(alt.toString()) catch unreachable;
        }

        return if_str.toOwnedSlice() catch unreachable;
    }
};

pub const FunctionLiteralExpression = struct {
    token: Token,
    parameters: []Identifier,
    body: *BlockStatement,

    pub fn toString(self: FunctionLiteralExpression) []const u8 {
        var func_str = std.ArrayList(u8).init(std.heap.page_allocator);
        defer func_str.deinit();

        func_str.appendSlice("fn (") catch unreachable;

        for (self.parameters, 0..) |param, i| {
            if (i > 0) {
                func_str.appendSlice(", ") catch unreachable;
            }
            func_str.appendSlice(param.toString()) catch unreachable;
        }

        func_str.appendSlice(") ") catch unreachable;
        func_str.appendSlice(self.body.toString()) catch unreachable;

        return func_str.toOwnedSlice() catch unreachable;
    }
};

pub const CallExpression = struct {
    token: Token,
    function: *Expression,
    arguments: []Expression,

    pub fn toString(self: CallExpression) []const u8 {
        var call_str = std.ArrayList(u8).init(std.heap.page_allocator);
        defer call_str.deinit();

        call_str.appendSlice(self.function.toString()) catch unreachable;
        call_str.appendSlice("(") catch unreachable;

        for (self.arguments, 0..) |arg, i| {
            if (i > 0) {
                call_str.appendSlice(", ") catch unreachable;
            }
            call_str.appendSlice(arg.toString()) catch unreachable;
        }

        call_str.appendSlice(")") catch unreachable;

        return call_str.toOwnedSlice() catch unreachable;
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

    pub fn node(self: Program) Node {
        return Node{ .statement = .{ .program = self } };
    }
};
