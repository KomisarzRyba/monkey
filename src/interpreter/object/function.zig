const std = @import("std");

const ast = @import("../../parser/ast.zig");
const Environment = @import("../environment.zig");

params: []ast.Identifier,
body: ast.BlockStatement,
env: *Environment,

const Self = @This();

pub fn init(params_in: []ast.Identifier, body_in: *ast.BlockStatement, env: *Environment) !Self {
    const arena = env.allocator;
    const allocator = arena.allocator();

    const body = try allocator.create(ast.BlockStatement);
    body.* = body_in.*;

    return Self{
        .params = try allocator.dupe(ast.Identifier, params_in),
        .body = body.*,
        .env = env,
    };
}

pub fn inspect(self: Self) []const u8 {
    var func_str = std.ArrayList(u8).init(std.heap.page_allocator);
    defer func_str.deinit();

    func_str.appendSlice("fn (") catch unreachable;

    for (self.params, 0..) |param, i| {
        if (i > 0) {
            func_str.appendSlice(", ") catch unreachable;
        }
        func_str.appendSlice(param.toString()) catch unreachable;
    }

    func_str.appendSlice(") ") catch unreachable;
    func_str.appendSlice(self.body.toString()) catch unreachable;

    return func_str.toOwnedSlice() catch unreachable;
}
