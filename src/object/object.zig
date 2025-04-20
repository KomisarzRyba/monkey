const Integer = @import("integer.zig");
const Boolean = @import("boolean.zig");

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    null,

    pub fn inspect(self: Object) []const u8 {
        return switch (self) {
            .integer => |i| i.inspect(),
            .boolean => |b| b.inspect(),
            .null => "null",
        };
    }
};
