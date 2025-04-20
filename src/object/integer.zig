const std = @import("std");

value: i64,

const Self = @This();

pub fn inspect(self: *Self) []const u8 {
    return std.fmt.allocPrint(
        std.heap.page_allocator,
        "{d}",
        .{self.value},
    ) catch unreachable;
}
