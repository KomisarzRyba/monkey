const std = @import("std");

const repl = @import("repl.zig");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader().any();
    const stdout = std.io.getStdOut().writer().any();

    var r = repl.Repl.init(stdin, stdout);
    r.start_msg =
        \\
        \\     w  c(..)o   (
        \\      \__(-)    __)
        \\          /\   (
        \\         /(_)___)
        \\         w /|
        \\          | \
        \\         m  m
        \\
    ;
    try r.run();
}
