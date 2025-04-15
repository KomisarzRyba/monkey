const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Token = @import("../lexer/token.zig");

pub const Expression = union(enum) {
    ident: *Identifier,
    // int: *IntegerLiteral,
    // prefix: *PrefixExpression,
    // infix: *InfixExpression,

    const Self = @This();

    pub fn node(self: Self) Node {
        return switch (self) {
            .ident => |e| e.node(),
            // .int => |e| e.node(),
            // .prefix => |e| e.node(),
            // .infix => |e| e.node(),
        };
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    const Self = @This();

    fn node(self: *Self) Node {
        return Node.init(self);
    }

    pub fn tokenLiteral(self: *Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *Self, allocator: Allocator) ![]const u8 {
        return try std.fmt.allocPrint(allocator, "{s}", .{self.value});
    }
};

pub const Statement = union(enum) {
    program: *Program,
    let: *LetStatement,
    // @"return": *ReturnStatement,
    // expr: *ExpressionStatement,

    const Self = @This();

    pub fn node(self: Statement) Node {
        return switch (self) {
            .program => |s| s.node(),
            .let => |s| s.node(),
            // .@"return" => |s| s.node(),
            // .expr => |s| s.node(),
        };
    }
};

pub const Program = struct {
    statements: []Statement,

    const Self = @This();

    fn node(self: *Self) Node {
        return Node.init(self);
    }

    pub fn tokenLiteral(self: *Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].node().tokenLit();
        }
        return "";
    }

    pub fn toString(self: *Program, allocator: Allocator) ![]const u8 {
        var str_buf = std.ArrayList(u8).init(allocator);
        for (self.statements) |stmt| {
            const str = try stmt.node().toString(allocator);
            try str_buf.appendSlice(str);
            allocator.free(str);
        }
        return try str_buf.toOwnedSlice();
    }
};

pub const LetStatement = struct {
    token: Token,
    name: *Identifier,
    value: Expression,

    const Self = @This();

    fn node(self: *Self) Node {
        return Node.init(self);
    }

    pub fn tokenLiteral(self: *Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *Self, allocator: Allocator) ![]const u8 {
        const value_str = try self.value.node().toString(allocator);
        defer allocator.free(value_str);
        return try std.fmt.allocPrint(allocator, "{s} {s} = {s};", .{
            self.tokenLiteral(),
            self.name.tokenLiteral(),
            value_str,
        });
    }
};

pub const Node = struct {
    const Self = @This();

    ptr: *anyopaque,
    tokenLiteralFn: *const fn (*anyopaque) []const u8,
    toStringFn: *const fn (*anyopaque, Allocator) anyerror![]const u8,

    pub fn init(pointer: anytype) Self {
        const Ptr = @TypeOf(pointer);
        assert(@typeInfo(Ptr) == .pointer);
        assert(@typeInfo(Ptr).pointer.size == .one);
        assert(@typeInfo(@typeInfo(Ptr).pointer.child) == .@"struct");

        const gen = struct {
            pub fn tokenLiteral(ptr: *anyopaque) []const u8 {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return @call(.always_inline, @typeInfo(Ptr).pointer.child.tokenLiteral, .{self});
            }

            pub fn toString(ptr: *anyopaque, allocator: Allocator) ![]const u8 {
                const self: Ptr = @ptrCast(@alignCast(ptr));
                return @call(.always_inline, @typeInfo(Ptr).pointer.child.toString, .{ self, allocator });
            }
        };

        return .{
            .ptr = pointer,
            .tokenLiteralFn = gen.tokenLiteral,
            .toStringFn = gen.toString,
        };
    }

    pub inline fn tokenLiteral(self: Self) []const u8 {
        return self.tokenLiteralFn(self.ptr);
    }

    pub inline fn toString(self: Self, allocator: Allocator) ![]const u8 {
        return self.toStringFn(self.ptr, allocator);
    }
};
