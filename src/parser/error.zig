const Token = @import("../lexer/token.zig");

pub const Error = error{
    UnexpectedToken,
};

err: Error,
location: Token.Location,

const Self = @This();

pub fn init(err: Error, location: Token.Location) Self {
    return .{
        .err = err,
        .location = location,
    };
}
