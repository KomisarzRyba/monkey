const std = @import("std");
const Token = @import("../lexer/token.zig");

pub const Error = error{
    UnexpectedToken,
};

err: Error,
location: Token.Location,
msg: ?[]const u8 = null,

const Self = @This();

pub fn init(
    err: Error,
    location: Token.Location,
) Self {
    return .{
        .err = err,
        .location = location,
    };
}

pub fn setMsg(self: *Self, msg: []const u8) void {
    self.msg = msg;
}

pub fn toString(self: Self) []const u8 {
    const kind: []const u8 = blk: {
        if (self.err == Error.UnexpectedToken) {
            break :blk "UnexpectedToken";
        } else unreachable;
    };
    const loc_str = self.location.toString();
    if (self.msg) |msg| {
        return std.fmt.allocPrint(
            std.heap.page_allocator,
            "{s}: {s} at {s}",
            .{ kind, msg, loc_str },
        ) catch unreachable;
    }
    return std.fmt.allocPrint(
        std.heap.page_allocator,
        "{s} at {s}",
        .{ kind, loc_str },
    ) catch unreachable;
}
