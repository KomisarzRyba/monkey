const std = @import("std");

const Boolean = @import("boolean.zig");
const Error = @import("error.zig");
const Integer = @import("integer.zig");
const String = @import("string.zig");
const Function = @import("function.zig");

pub const Object = union(enum) {
    integer: Integer,
    string: String,
    boolean: *const Boolean,
    null,
    @"return": *Object,
    @"error": Error,
    function: Function,

    const Self = @This();

    pub fn inspect(self: Self) []const u8 {
        return switch (self) {
            .integer => |i| i.inspect(),
            .string => |s| s.inspect(),
            .boolean => |b| b.inspect(),
            .null => "null",
            .@"return" => |r| r.*.inspect(),
            .@"error" => |e| e.inspect(),
            .function => |f| f.inspect(),
        };
    }

    pub fn eq(self: Self, other: Self) bool {
        return switch (self) {
            .null => .null == std.meta.activeTag(other),
            .@"error" => unreachable,
            .function => unreachable,
            inline else => |value, tag| tag == std.meta.activeTag(other) and value.eq(other),
        };
    }

    pub fn truthy(self: Object) bool {
        return switch (self) {
            .integer => |i| i.truthy(),
            .string => |s| s.truthy(),
            .boolean => |b| b.truthy(),
            .null => false,
            .@"return" => |r| r.*.truthy(),
            else => unreachable,
        };
    }
};
