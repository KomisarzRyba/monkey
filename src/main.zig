const std = @import("std");

const Repl = @import("repl.zig");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader().any();
    const stdout = std.io.getStdOut().writer().any();

    var r = Repl.init(stdin, stdout);
    r.setStartMsg(
        \\
        \\     w  c(..)o   (
        \\      \__(-)    __)
        \\          /\   (
        \\         /(_)___)
        \\         w /|
        \\          | \
        \\         m  m
        \\
    );
    try r.run();
}
