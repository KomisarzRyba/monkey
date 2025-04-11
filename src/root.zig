const std = @import("std");

pub const Lexer = @import("lexer/lexer.zig").Lexer;

test {
    std.testing.refAllDecls(@This());
}
