const Integer = @import("integer.zig");
const Boolean = @import("boolean.zig");

pub const Object = union(enum) {
    integer: Integer,
    boolean: *const Boolean,
    null,
    @"return": *Object,

    pub fn inspect(self: Object) []const u8 {
        return switch (self) {
            .integer => |i| i.inspect(),
            .boolean => |b| b.inspect(),
            .null => "null",
            .@"return" => |r| r.*.inspect(),
        };
    }

    pub fn eq(self: Object, other: Object) bool {
        return switch (self) {
            .integer => |i| i.eq(other),
            .boolean => |b| b.eq(other),
            .null => self == .null and other == .null,
            .@"return" => |r| r.*.eq(other),
        };
    }

    pub fn truthy(self: Object) bool {
        return switch (self) {
            .integer => |i| i.truthy(),
            .boolean => |b| b.truthy(),
            .null => false,
            .@"return" => |r| r.*.truthy(),
        };
    }
};
