const std = @import("std");

const repl = @import("repl/repl.zig");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader().any();
    const stdout = std.io.getStdOut().writer().any();

    const r = repl.Repl.init(stdin, stdout);
    try r.run();
}
