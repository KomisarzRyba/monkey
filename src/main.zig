const std = @import("std");

const Lexer = @import("lexer/lexer.zig");
const Parser = @import("parser/parser.zig");
const Environment = @import("interpreter/environment.zig");
const eval = @import("interpreter/interpreter.zig").eval;

const Repl = @import("repl.zig");

pub fn main() !void {
    var args_iter = std.process.args();

    const bin_name = args_iter.next().?;
    _ = bin_name;

    const maybe_path = args_iter.next();
    if (maybe_path == null) {
        return repl();
    }
    const path = maybe_path.?;
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        return std.debug.print(
            "Failed to open {s}: {s}",
            .{ path, @errorName(err) },
        );
    };
    defer file.close();

    const allocator = std.heap.page_allocator;
    const monkey = try file.readToEndAlloc(allocator, std.math.maxInt(u32));

    var env_arena = std.heap.ArenaAllocator.init(allocator);
    defer env_arena.deinit();

    const env_alloc = env_arena.allocator();

    var env = Environment.init(env_alloc);
    defer env.deinit();

    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();

    const parser_alloc = ast_arena.allocator();
    var lexer = Lexer.init(monkey);
    var parser = Parser.init(parser_alloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();
    const evaluated = try eval(program.*.node(), &env);
    std.debug.print("{s}\n", .{evaluated.inspect()});

    for (parser.getErrors()) |err| {
        std.debug.print("| Error: {s}\n", .{err.toString()});
    }
}

fn repl() !void {
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
