const std = @import("std");

const Lexer = @import("lexer/lexer.zig");
const Parser = @import("parser/parser.zig");
const interpreter = @import("interpreter/interpreter.zig");
const Environment = @import("interpreter/environment.zig");

in: std.io.AnyReader,
out: std.io.AnyWriter,
prompt: []const u8 = "> ",
start_msg: ?[]const u8 = null,

const Self = @This();

pub fn init(in: std.io.AnyReader, out: std.io.AnyWriter) Self {
    return Self{
        .in = in,
        .out = out,
    };
}

pub fn setPrompt(self: *Self, prompt: []const u8) void {
    self.prompt = prompt;
}

pub fn setStartMsg(self: *Self, msg: []const u8) void {
    self.start_msg = msg;
}

pub fn run(self: Self) !void {
    if (self.start_msg) |msg| {
        try self.out.print("{s}\n", .{msg});
    }

    const allocator = std.heap.page_allocator;

    while (true) {
        try self.out.print("{s}", .{self.prompt});

        var line_buf = try std.BoundedArray(u8, 1024).init(0);

        self.in.streamUntilDelimiter(line_buf.writer(), '\n', 1024) catch |err| {
            if (err == std.io.AnyReader.Error.EndOfStream) {
                return;
            }
            return err;
        };

        var lexer = Lexer.init(&line_buf.buffer);
        var parser = Parser.init(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parseProgram();

        var env = Environment.init(allocator);
        const evaluated = try interpreter.eval(program.*.node(), &env);
        try self.out.print("{s}\n", .{evaluated.inspect()});

        for (parser.getErrors()) |err| {
            try self.out.print("| Error: {s}\n", .{err.toString()});
        }
    }
}
