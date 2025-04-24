const std = @import("std");
const Object = @import("object.zig").Object;

value: bool,

const Self = @This();

pub fn inspect(self: Self) []const u8 {
    return if (self.value) "true" else "false";
}

pub fn eq(self: *const Self, other: Object) bool {
    return switch (other) {
        .integer => false,
        .boolean => |b| self == b,
        .null => false,
    };
}

pub fn truthy(self: *const Self) bool {
    return self.value;
}

pub const True = Self{ .value = true };
pub const False = Self{ .value = false };
