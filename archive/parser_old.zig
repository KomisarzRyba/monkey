const std = @import("std");
const testing = std.testing;

const Lexer = @import("../lexer/lexer.zig");
const Token = @import("../lexer/token.zig");
const ast = @import("ast.zig");
const ParsingError = @import("error.zig");

allocator: std.mem.Allocator,
lexer: *Lexer,
cur_tok: Token = undefined,
peek_tok: Token = undefined,
errors: std.ArrayList(ParsingError) = undefined,

const Self = @This();

pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) Self {
    return Self{
        .allocator = allocator,
        .lexer = lexer,
        .cur_tok = lexer.next(),
        .peek_tok = lexer.next(),
        .errors = std.ArrayList(ParsingError).init(allocator),
    };
}

pub fn deinit(self: *Self, program: *ast.Program) void {
    for (program.statements) |stmt| {
        switch (stmt) {
            .let => |s| {
                self.allocator.destroy(s.name);
                self.allocator.destroy(s);
            },
            .program => unreachable,
        }
    }
    self.allocator.free(program.statements);
    self.allocator.destroy(program);
}

pub fn parseProgram(self: *Self) !*ast.Program {
    var program = try self.allocator.create(ast.Program);
    var statements = std.ArrayList(ast.Statement).init(self.allocator);
    errdefer statements.deinit();
    while (self.cur_tok.token_type != .eof) {
        const stmt = try self.parseStatement();
        if (stmt) |s| {
            try statements.append(s);
        }
        self.nextToken();
    }
    program.statements = try statements.toOwnedSlice();
    return program;
}

fn parseStatement(self: *Self) !?ast.Statement {
    return switch (self.cur_tok.token_type) {
        .let => .{ .let = try self.parseLetStatement() orelse return null },
        else => unreachable,
    };
}

fn parseLetStatement(self: *Self) !?*ast.LetStatement {
    var stmt = try self.allocator.create(ast.LetStatement);
    errdefer self.allocator.destroy(stmt);

    stmt.token = self.cur_tok;

    if (!self.expectPeek(.ident)) {
        self.allocator.destroy(stmt);
        return null;
    }

    stmt.name = try self.allocator.create(ast.Identifier);
    errdefer self.allocator.destroy(stmt.name);
    stmt.name.* = .{
        .token = self.cur_tok,
        .value = self.cur_tok.literal,
    };

    if (!self.expectPeek(.assign)) {
        return null;
    }

    while (self.cur_tok.token_type != .semicolon) {
        self.nextToken();
    }

    return stmt;
}

fn nextToken(self: *Self) void {
    self.cur_tok = self.peek_tok;
    self.peek_tok = self.lexer.next();
}

fn isCurTokenType(self: *Self, token_type: Token.Type) bool {
    return self.cur_tok.token_type == token_type;
}

fn isPeekTokenType(self: *Self, token_type: Token.Type) bool {
    return self.peek_tok.token_type == token_type;
}

/// Expect the current token to be of the given type.
/// If it is, consume the token and return true.
/// Otherwise, register an error and return false.
fn expectCur(self: *Self, token_type: Token.Type) bool {
    if (self.isCurTokenType(token_type)) {
        self.nextToken();
        return true;
    } else {
        self.pushError(
            ParsingError.Error.UnexpectedToken,
            self.cur_tok.location,
        );
        return false;
    }
}

/// Expect the next token to be of the given type.
/// If it is, consume the token and return true.
/// Otherwise, register an error and return false.
fn expectPeek(self: *Self, token_type: Token.Type) bool {
    if (self.isPeekTokenType(token_type)) {
        self.nextToken();
        return true;
    } else {
        self.pushError(
            ParsingError.Error.UnexpectedToken,
            self.peek_tok.location,
        );
        return false;
    }
}

fn pushError(self: *Self, err: ParsingError.Error, location: Token.Location) void {
    self.errors.append(ParsingError.init(err, location)) catch unreachable;
}

test "test let statements" {
    const allocator = testing.allocator;
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var lexer = Lexer.init(input);
    var p = init(allocator, &lexer);

    const program = try p.parseProgram();
    defer p.deinit(program);

    try testing.expectEqual(3, program.statements.len);

    const expected_idents = [_][]const u8{
        "x",
        "y",
        "foobar",
    };

    for (expected_idents, 0..) |ident, i| {
        const stmt = program.statements[i];
        std.debug.print("{any}", .{stmt.node().toString(testing.allocator)});
        try testing.expectEqualStrings(ident, stmt.let.name.value);
    }
}
