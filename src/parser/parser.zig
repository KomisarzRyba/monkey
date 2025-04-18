const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const Lexer = @import("../lexer/lexer.zig");
const Token = @import("../lexer/token.zig");
const ast = @import("ast.zig");
const ParsingError = @import("errors.zig");

lexer: *Lexer,
allocator: Allocator,
cur_token: Token,
peek_token: Token,
errors: std.ArrayList(ParsingError),

const Self = @This();

pub fn init(allocator: Allocator, lexer: *Lexer) Self {
    return Self{
        .lexer = lexer,
        .allocator = allocator,
        .cur_token = lexer.next(),
        .peek_token = lexer.next(),
        .errors = std.ArrayList(ParsingError).init(allocator),
    };
}

pub fn deinit(self: *Self, program: *ast.Program) void {
    if (program.statements.len > 0) {
        self.allocator.free(program.statements);
    }
    self.allocator.destroy(program);
    self.errors.deinit();
}

pub fn parseProgram(self: *Self) !*ast.Program {
    var program = try self.allocator.create(ast.Program);
    var statements = std.ArrayList(ast.Statement).init(self.allocator);
    errdefer statements.deinit();

    while (self.cur_token.token_type != .eof) {
        const stmt = try self.parseStatement();
        if (stmt) |s| {
            try statements.append(s);
        }
        self.nextToken();
    }

    program.statements = try statements.toOwnedSlice();
    return program;
}

pub fn getErrors(self: *Self) []ParsingError {
    return self.errors.items;
}

fn nextToken(self: *Self) void {
    self.cur_token = self.peek_token;
    self.peek_token = self.lexer.next();
}

fn parseStatement(self: *Self) !?ast.Statement {
    return switch (self.cur_token.token_type) {
        .let => .{ .let = try self.parseLetStatement() orelse return null },
        .@"return" => .{ .@"return" = try self.parseReturnStatement() orelse return null },
        else => null,
    };
}

fn parseLetStatement(self: *Self) !?ast.LetStatement {
    const let_token = self.cur_token;

    if (!self.expectPeek(.ident)) {
        return null;
    }
    const name = ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    };

    if (!self.expectPeek(.assign)) {
        return null;
    }
    // TODO: for now, just skip to the semicolon
    while (!self.curTokenIs(.semicolon)) {
        self.nextToken();
    }

    return ast.LetStatement{
        .token = let_token,
        .name = name,
        .value = undefined, // TODO: parse the value
    };
}

fn parseReturnStatement(self: *Self) !?ast.ReturnStatement {
    const return_token = self.cur_token;

    self.nextToken();

    // TODO: for now, just skip to the semicolon
    while (!self.curTokenIs(.semicolon)) {
        self.nextToken();
    }

    return ast.ReturnStatement{
        .token = return_token,
        .return_value = undefined, // TODO: parse the return value
    };
}

fn curTokenIs(self: *Self, token_type: Token.Type) bool {
    return self.cur_token.token_type == token_type;
}

fn peekTokenIs(self: *Self, token_type: Token.Type) bool {
    return self.peek_token.token_type == token_type;
}

fn expectPeek(self: *Self, token_type: Token.Type) bool {
    if (self.peekTokenIs(token_type)) {
        self.nextToken();
        return true;
    }
    self.peekError(token_type) catch unreachable;
    return false;
}

fn peekError(self: *Self, expected_token_type: Token.Type) !void {
    const msg = try std.fmt.allocPrint(
        self.allocator,
        "expected next token to be {s}, got {s} instead",
        .{
            expected_token_type.name(),
            self.peek_token.token_type.name(),
        },
    );

    var err = ParsingError.init(
        ParsingError.Error.UnexpectedToken,
        self.peek_token.location,
    );
    err.setMsg(msg);

    try self.errors.append(err);
}

test "let statements" {
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

    const errors = p.getErrors();
    if (errors.len > 0) {
        for (errors) |err| {
            std.debug.print("parser error: {s}\n", .{err.toString()});
        }
    }
    try testing.expectEqual(3, program.statements.len);

    const expected_idents = [_][]const u8{
        "x",
        "y",
        "foobar",
    };

    for (expected_idents, 0..) |ident, i| {
        const stmt = program.statements[i];
        try testing.expectEqualStrings(ident, stmt.let.name.value);
    }
}

test "return statements" {
    const allocator = testing.allocator;
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;
    var lexer = Lexer.init(input);
    var p = init(allocator, &lexer);

    const program = try p.parseProgram();
    defer p.deinit(program);

    try testing.expectEqual(3, program.statements.len);
}
