const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const Lexer = @import("../lexer/lexer.zig");
const Token = @import("../lexer/token.zig");
const ast = @import("ast.zig");
const ParsingError = @import("errors.zig");

lexer: *Lexer,
arena: std.heap.ArenaAllocator,
cur_token: Token,
peek_token: Token,
errors: std.ArrayList(ParsingError),
prefix_parse_fns: std.AutoHashMap(Token.Type, *const PrefixParseFn),
infix_parse_fns: std.AutoHashMap(Token.Type, *const InfixParseFn),

const Self = @This();

pub fn init(allocator: Allocator, lexer: *Lexer) Self {
    var self = Self{
        .lexer = lexer,
        .arena = .init(allocator),
        .cur_token = lexer.next(),
        .peek_token = lexer.next(),
        .errors = .init(allocator),
        .prefix_parse_fns = .init(allocator),
        .infix_parse_fns = .init(allocator),
    };

    self.registerPrefix(.ident, parseIdentifier) catch unreachable;
    self.registerPrefix(.int, parseIntegerLiteral) catch unreachable;
    self.registerPrefix(.minus, parsePrefixExpression) catch unreachable;
    self.registerPrefix(.bang, parsePrefixExpression) catch unreachable;
    self.registerPrefix(.true, parseBooleanLiteral) catch unreachable;
    self.registerPrefix(.false, parseBooleanLiteral) catch unreachable;
    self.registerPrefix(.lparen, parseGroupedExpression) catch unreachable;

    self.registerInfix(.plus, parseInfixExpression) catch unreachable;
    self.registerInfix(.minus, parseInfixExpression) catch unreachable;
    self.registerInfix(.asterisk, parseInfixExpression) catch unreachable;
    self.registerInfix(.slash, parseInfixExpression) catch unreachable;
    self.registerInfix(.lt, parseInfixExpression) catch unreachable;
    self.registerInfix(.gt, parseInfixExpression) catch unreachable;
    self.registerInfix(.eq, parseInfixExpression) catch unreachable;
    self.registerInfix(.not_eq, parseInfixExpression) catch unreachable;

    return self;
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.errors.deinit();
    self.prefix_parse_fns.deinit();
    self.infix_parse_fns.deinit();
}

pub fn parseProgram(self: *Self) !*ast.Program {
    const allocator = self.arena.allocator();
    var program = try allocator.create(ast.Program);
    var statements = std.ArrayList(ast.Statement).init(allocator);
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
        else => .{ .expression = try self.parseExpressionStatement() orelse return null },
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
        // TODO: handle error
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

fn parseExpressionStatement(self: *Self) !?ast.ExpressionStatement {
    const expr_token = self.cur_token;

    const expr = try self.parseExpression(.lowest) orelse return null;

    if (self.peekTokenIs(.semicolon)) {
        self.nextToken();
    }

    return ast.ExpressionStatement{
        .token = expr_token,
        .expression = expr,
    };
}

fn parseExpression(self: *Self, precedence: Precedence) !?ast.Expression {
    const prefix_fn = self.prefix_parse_fns.get(self.cur_token.token_type) orelse return null;
    var left_expr = try prefix_fn(self) orelse return null;

    while (!self.peekTokenIs(.semicolon) and precedence.lt(self.peekPrecdence())) {
        if (self.infix_parse_fns.get(self.peek_token.token_type)) |infix_fn| {
            self.nextToken();
            left_expr = try infix_fn(self, left_expr) orelse return null;
        } else break;
    }

    return left_expr;
}

fn parseIdentifier(self: *Self) !?ast.Expression {
    return ast.Expression{
        .identifier = ast.Identifier{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        },
    };
}

fn parseIntegerLiteral(self: *Self) !?ast.Expression {
    const value = std.fmt.parseInt(i64, self.cur_token.literal, 10) catch unreachable;

    return ast.Expression{
        .integer_literal = ast.IntegerLiteral{
            .token = self.cur_token,
            .value = value,
        },
    };
}

fn parsePrefixExpression(self: *Self) !?ast.Expression {
    const prefix_token = self.cur_token;
    self.nextToken();

    const right_expr = try self.arena.allocator().create(ast.Expression);
    right_expr.* = try self.parseExpression(.prefix) orelse return null;

    return ast.Expression{
        .prefix = ast.Prefix{
            .token = prefix_token,
            .operator = ast.PrefixOperator.fromTokenType(prefix_token.token_type),
            .right = right_expr,
        },
    };
}

fn parseInfixExpression(self: *Self, left: ast.Expression) !?ast.Expression {
    const infix_token = self.cur_token;
    const precedence = self.curPrecdence();

    self.nextToken();

    const left_expr = try self.arena.allocator().create(ast.Expression);
    left_expr.* = left;

    const right_expr = try self.arena.allocator().create(ast.Expression);
    right_expr.* = try self.parseExpression(precedence) orelse return null;

    return ast.Expression{
        .infix = ast.Infix{
            .token = infix_token,
            .left = left_expr,
            .operator = ast.InfixOperator.fromTokenType(infix_token.token_type),
            .right = right_expr,
        },
    };
}

fn parseBooleanLiteral(self: *Self) !?ast.Expression {
    return ast.Expression{
        .boolean_literal = ast.BooleanLiteral{
            .token = self.cur_token,
            .value = self.cur_token.token_type == .true,
        },
    };
}

fn parseGroupedExpression(self: *Self) !?ast.Expression {
    self.nextToken();

    const expr = try self.parseExpression(.lowest);

    if (!self.expectPeek(.rparen)) {
        return null;
    }

    return expr;
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

fn peekPrecdence(self: *Self) Precedence {
    return Precedence.from_token_type(self.peek_token.token_type);
}

fn curPrecdence(self: *Self) Precedence {
    return Precedence.from_token_type(self.cur_token.token_type);
}

fn peekError(self: *Self, expected_token_type: Token.Type) !void {
    const msg = try std.fmt.allocPrint(
        self.arena.allocator(),
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

const Precedence = enum {
    lowest,
    equals,
    lessGreater,
    sum,
    product,
    prefix,
    call,

    fn from_token_type(token_type: Token.Type) Precedence {
        return switch (token_type) {
            .eq, .not_eq => .equals,
            .lt, .gt => .lessGreater,
            .plus, .minus => .sum,
            .asterisk, .slash => .product,
            else => .lowest,
        };
    }

    fn lt(self: Precedence, other: Precedence) bool {
        return @intFromEnum(self) < @intFromEnum(other);
    }

    fn gt(self: Precedence, other: Precedence) bool {
        return @intFromEnum(self) > @intFromEnum(other);
    }
};

const PrefixParseFn = fn (self: *Self) anyerror!?ast.Expression;
const InfixParseFn = fn (self: *Self, left_side: ast.Expression) anyerror!?ast.Expression;

fn registerPrefix(self: *Self, token_type: Token.Type, parse_fn: *const PrefixParseFn) !void {
    try self.prefix_parse_fns.put(token_type, parse_fn);
}

fn registerInfix(self: *Self, token_type: Token.Type, parse_fn: *const InfixParseFn) !void {
    try self.infix_parse_fns.put(token_type, parse_fn);
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
    defer p.deinit();

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

    std.debug.print("program:\n{s}\n", .{program.toString()});
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
    defer p.deinit();

    try testing.expectEqual(3, program.statements.len);

    std.debug.print("program:\n{s}\n", .{program.toString()});
}

test "identifier expression" {
    const allocator = testing.allocator;
    const input = "foobar;";
    var lexer = Lexer.init(input);
    var p = init(allocator, &lexer);

    const program = try p.parseProgram();
    defer p.deinit();

    if (p.getErrors().len > 0) {
        for (p.getErrors()) |err| {
            std.debug.print("parser error: {s}\n", .{err.toString()});
        }
    }

    try testing.expectEqual(1, program.statements.len);

    const stmt = program.statements[0];
    try testing.expectEqualStrings("foobar", stmt.expression.expression.identifier.token.literal);
    try testing.expectEqualStrings("foobar", stmt.expression.expression.identifier.value);
}

test "integer literal expression" {
    const allocator = testing.allocator;
    const input = "5;";
    var lexer = Lexer.init(input);
    var p = init(allocator, &lexer);

    const program = try p.parseProgram();
    defer p.deinit();

    if (p.getErrors().len > 0) {
        for (p.getErrors()) |err| {
            std.debug.print("parser error: {s}\n", .{err.toString()});
        }
    }

    try testing.expectEqual(1, program.statements.len);

    const stmt = program.statements[0];
    try testLiteralExpression(stmt.expression.expression, 5);

    std.debug.print("program:\n{s}\n", .{program.toString()});
}

test "prefix expressions" {
    const tests = [_]struct {
        input: []const u8,
        operator: ast.PrefixOperator,
        value: i64,
    }{
        .{ .input = "!5;", .operator = .bang, .value = 5 },
        .{ .input = "-15;", .operator = .minus, .value = 15 },
    };

    const allocator = testing.allocator;

    for (tests) |t| {
        var lexer = Lexer.init(t.input);
        var p = init(allocator, &lexer);

        const program = try p.parseProgram();
        defer p.deinit();

        if (p.getErrors().len > 0) {
            for (p.getErrors()) |err| {
                std.debug.print("parser error: {s}\n", .{err.toString()});
            }
        }

        try testing.expectEqual(1, program.statements.len);

        const stmt = program.statements[0];
        try testing.expectEqual(t.operator, stmt.expression.expression.prefix.operator);
        try testLiteralExpression(stmt.expression.expression.prefix.right.*, t.value);

        std.debug.print("program:\n{s}\n", .{program.toString()});
    }
}

test "infix expressions" {
    const tests = [_]struct {
        input: []const u8,
        left_value: i64,
        operator: ast.InfixOperator,
        right_value: i64,
    }{
        .{ .input = "5 + 5;", .left_value = 5, .operator = ast.InfixOperator.plus, .right_value = 5 },
        .{ .input = "5 - 5;", .left_value = 5, .operator = ast.InfixOperator.minus, .right_value = 5 },
        .{ .input = "5 * 5;", .left_value = 5, .operator = ast.InfixOperator.asterisk, .right_value = 5 },
        .{ .input = "5 / 5;", .left_value = 5, .operator = ast.InfixOperator.slash, .right_value = 5 },
        .{ .input = "5 < 5;", .left_value = 5, .operator = ast.InfixOperator.lt, .right_value = 5 },
        .{ .input = "5 > 5;", .left_value = 5, .operator = ast.InfixOperator.gt, .right_value = 5 },
        .{ .input = "5 == 5;", .left_value = 5, .operator = ast.InfixOperator.eq, .right_value = 5 },
        .{ .input = "5 != 5;", .left_value = 5, .operator = ast.InfixOperator.not_eq, .right_value = 5 },
    };

    const allocator = testing.allocator;

    for (tests) |t| {
        var lexer = Lexer.init(t.input);
        var p = init(allocator, &lexer);

        const program = try p.parseProgram();
        defer p.deinit();

        std.debug.print("program:\n{s}\n", .{program.toString()});

        if (p.getErrors().len > 0) {
            for (p.getErrors()) |err| {
                std.debug.print("parser error: {s}\n", .{err.toString()});
            }
        }

        try testing.expectEqual(1, program.statements.len);

        const stmt = program.statements[0];
        try testInfixExpression(stmt.expression.expression, t.left_value, t.operator, t.right_value);
    }
}

test "boolean literal expression" {
    const allocator = testing.allocator;
    const input = "true; false;";
    var lexer = Lexer.init(input);
    var p = init(allocator, &lexer);

    const program = try p.parseProgram();
    defer p.deinit();

    if (p.getErrors().len > 0) {
        for (p.getErrors()) |err| {
            std.debug.print("parser error: {s}\n", .{err.toString()});
        }
    }

    try testing.expectEqual(2, program.statements.len);
    const expected_values = [_]bool{ true, false };
    for (expected_values, 0..) |value, i| {
        const stmt = program.statements[i];
        try testing.expectEqual(value, stmt.expression.expression.boolean_literal.value);
    }
    std.debug.print("program:\n{s}\n", .{program.toString()});
}

test "operator precedence parsing" {
    const tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{ .input = "-a * b;", .expected = "((-a) * b)" },
        .{ .input = "!-a;", .expected = "(!(-a))" },
        .{ .input = "a + b + c;", .expected = "((a + b) + c)" },
        .{ .input = "a + b - c;", .expected = "((a + b) - c)" },
        .{ .input = "a * b * c;", .expected = "((a * b) * c)" },
        .{ .input = "a * b / c;", .expected = "((a * b) / c)" },
        .{ .input = "a + b / c;", .expected = "(a + (b / c))" },
        .{ .input = "a + b * c + d / e - f;", .expected = "(((a + (b * c)) + (d / e)) - f)" },
        .{ .input = "3 + 4; -5 * 5;", .expected = "(3 + 4)((-5) * 5)" },
        .{ .input = "5 > 4 == 3 < 4;", .expected = "((5 > 4) == (3 < 4))" },
        .{ .input = "5 < 4 != 3 > 4;", .expected = "((5 < 4) != (3 > 4))" },
        .{ .input = "3 + 4 * 5 == 3 * 1 + 4 * 5;", .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
        .{ .input = "3 > 5 == false;", .expected = "((3 > 5) == false)" },
        .{ .input = "3 < 5 == true;", .expected = "((3 < 5) == true)" },
        .{ .input = "1 + (2 + 3) + 4;", .expected = "((1 + (2 + 3)) + 4)" },
        .{ .input = "(5 + 5) * 2;", .expected = "((5 + 5) * 2)" },
        .{ .input = "2 / (5 + 5);", .expected = "(2 / (5 + 5))" },
        .{ .input = "-(5 + 5);", .expected = "(-(5 + 5))" },
        .{ .input = "!(true == true);", .expected = "(!(true == true))" },
    };

    const allocator = testing.allocator;
    for (tests) |t| {
        var lexer = Lexer.init(t.input);
        var p = init(allocator, &lexer);

        const program = try p.parseProgram();
        defer p.deinit();

        if (p.getErrors().len > 0) {
            for (p.getErrors()) |err| {
                std.debug.print("parser error: {s}\n", .{err.toString()});
            }
        }

        const actual = program.toString();
        try testing.expectEqualStrings(t.expected, actual);

        std.debug.print("program:\n{s}\n", .{actual});
    }
}

fn testInfixExpression(
    expr: ast.Expression,
    left: anytype,
    operator: ast.InfixOperator,
    right: anytype,
) !void {
    const infix = expr.infix;
    try testLiteralExpression(infix.left.*, left);
    try testing.expectEqual(operator, infix.operator);
    try testLiteralExpression(infix.right.*, right);
}

fn testLiteralExpression(expr: ast.Expression, expected: anytype) !void {
    switch (@typeInfo(@TypeOf(expected))) {
        .int, .comptime_int => try testIntegerLiteral(expr, expected),
        .array => |info| {
            std.debug.print("array type: {any}\n", .{info});
        },
        .bool => try testBooleanLiteral(expr, expected),
        else => |info| {
            std.debug.print("unexpected type: {any}\n", .{info});
        },
    }
}

fn testIntegerLiteral(expr: ast.Expression, value: i64) !void {
    const int_lit = expr.integer_literal;
    try testing.expectEqual(.int, int_lit.token.token_type);
    try testing.expectEqual(value, int_lit.value);
}

fn testIdentifier(expr: ast.Expression, value: []const u8) !void {
    const ident = expr.identifier;
    try testing.expectEqual(.ident, ident.token.token_type);
    try testing.expectEqualStrings(value, ident.value);
}

fn testBooleanLiteral(expr: ast.Expression, value: bool) !void {
    const bool_lit = expr.boolean_literal;
    try testing.expectEqual(.true, bool_lit.token.token_type);
    try testing.expectEqual(value, bool_lit.value);
}
