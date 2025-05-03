const std = @import("std");
const Object = @import("object.zig").Object;

value: []const u8,

const Self = @This();

pub fn inspect(self: Self) []const u8 {
    return std.fmt.allocPrint(
        std.heap.page_allocator,
        "\"{s}\"",
        .{self.value},
    ) catch unreachable;
}

pub fn eq(self: Self, other: Object) bool {
    return switch (other) {
        .integer => false,
        .string => |s| std.mem.eql(u8, self.value, s.value),
        .null => false,
        .@"return" => |r| self.eq(r.*),
        .boolean => false,
        else => unreachable,
    };
}

pub fn truthy(self: Self) bool {
    return self.value.len > 0;
}

