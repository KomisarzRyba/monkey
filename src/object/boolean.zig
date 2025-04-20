const std = @import("std");

value: bool,

const Self = @This();

pub fn inspect(self: Self) []const u8 {
    return if (self.value) "true" else "false";
}

pub const True = Self{ .value = true };
pub const False = Self{ .value = false };
