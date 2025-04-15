const std = @import("std");

const Lexer = @import("../lexer/lexer.zig");

pub const Repl = struct {
    in: std.io.AnyReader,
    out: std.io.AnyWriter,
    prompt: []const u8,

    const Self = @This();

    pub fn init(in: std.io.AnyReader, out: std.io.AnyWriter) Self {
        return Self{
            .in = in,
            .out = out,
            .prompt = "> ",
        };
    }

    pub fn setPrompt(self: *Self, prompt: []const u8) void {
        self.prompt = prompt;
    }

    pub fn run(self: Self) !void {
        while (true) {
            try self.out.print("{s}", .{self.prompt});

            var line_buf = try std.BoundedArray(u8, 1024).init(0);

            self.in.streamUntilDelimiter(line_buf.writer(), '\n', 1024) catch |err| {
                if (err == std.io.AnyReader.Error.EndOfStream) {
                    return;
                }
                return err;
            };

            try self.out.print("{s}\n", .{line_buf.buffer});

            var lexer = Lexer.init(&line_buf.buffer);
            while (true) {
                const tok = lexer.next();
                try self.out.print("{}\n", .{tok});
                if (tok.token_type == .eof) {
                    break;
                }
            }
        }
    }
};
