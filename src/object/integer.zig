const std = @import("std");
const Object = @import("object.zig").Object;

value: i64,

const Self = @This();

pub fn inspect(self: Self) []const u8 {
    return std.fmt.allocPrint(
        std.heap.page_allocator,
        "{d}",
        .{self.value},
    ) catch unreachable;
}

pub fn eq(self: Self, other: Object) bool {
    return switch (other) {
        .integer => |i| self.value == i.value,
        .boolean => false,
        .null => false,
    };
}
