const std = @import("std");

message: []const u8,

const Self = @This();

pub fn new(comptime fmt: []const u8, args: anytype) Self {
    return Self{
        .message = std.fmt.allocPrint(
            std.heap.page_allocator,
            fmt,
            args,
        ) catch unreachable,
    };
}

pub fn inspect(self: Self) []const u8 {
    return std.fmt.allocPrint(
        std.heap.page_allocator,
        "Whoops! Error: {s}",
        .{self.message},
    ) catch unreachable;
}
