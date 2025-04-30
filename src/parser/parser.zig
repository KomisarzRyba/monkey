const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const Lexer = @import("../lexer/lexer.zig");
const Token = @import("../lexer/token.zig");
const ast = @import("ast.zig");
const ParsingError = @import("errors.zig");

lexer: *Lexer,
alloc: Allocator,
cur_token: Token,
peek_token: Token,
errors: std.ArrayList(ParsingError),

const Self = @This();

pub fn init(allocator: Allocator, lexer: *Lexer) Self {
    return Self{
        .lexer = lexer,
        .alloc = allocator,
        .cur_token = lexer.next(),
        .peek_token = lexer.next(),
        .errors = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.errors.deinit();
}

pub fn parseProgram(self: *Self) !*ast.Program {
    var program = try self.alloc.create(ast.Program);
    var statements = std.ArrayList(ast.Statement).init(self.alloc);
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
        return null;
    }
    self.nextToken();

    const value = try self.parseExpression(.lowest) orelse return null;

    if (self.peekTokenIs(.semicolon)) {
        self.nextToken();
    }

    return ast.LetStatement{
        .token = let_token,
        .name = name,
        .value = value,
    };
}

fn parseReturnStatement(self: *Self) !?ast.ReturnStatement {
    const return_token = self.cur_token;

    self.nextToken();

    const value = try self.parseExpression(.lowest) orelse return null;

    if (self.peekTokenIs(.semicolon)) {
        self.nextToken();
    }

    return ast.ReturnStatement{
        .token = return_token,
        .return_value = value,
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

fn parseBlockStatement(self: *Self) !?ast.BlockStatement {
    const block_token = self.cur_token;

    self.nextToken();

    var statements = std.ArrayList(ast.Statement).init(self.alloc);

    while (!self.curTokenIs(.rbrace) and !self.curTokenIs(.eof)) {
        if (try self.parseStatement()) |stmt| {
            try statements.append(stmt);
        }
        self.nextToken();
    }

    return ast.BlockStatement{
        .token = block_token,
        .statements = try statements.toOwnedSlice(),
    };
}

fn parseExpression(self: *Self, precedence: Precedence) !?ast.Expression {
    const prefix_fn = getPrefixParseFn(self.cur_token.token_type) orelse return null;
    var left_expr = try prefix_fn(self) orelse return null;

    while (!self.peekTokenIs(.semicolon) and precedence.lt(self.peekPrecdence())) {
        if (getInfixParseFn(self.peek_token.token_type)) |infix_fn| {
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
        .integer_literal = ast.IntegerLiteralExpression{
            .token = self.cur_token,
            .value = value,
        },
    };
}

fn parsePrefixExpression(self: *Self) !?ast.Expression {
    const prefix_token = self.cur_token;
    self.nextToken();

    const right_expr = try self.alloc.create(ast.Expression);
    right_expr.* = try self.parseExpression(.prefix) orelse return null;

    return ast.Expression{
        .prefix = ast.PrefixExpression{
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

    const left_expr = try self.alloc.create(ast.Expression);
    left_expr.* = left;

    const right_expr = try self.alloc.create(ast.Expression);
    right_expr.* = try self.parseExpression(precedence) orelse return null;

    return ast.Expression{
        .infix = ast.InfixExpression{
            .token = infix_token,
            .left = left_expr,
            .operator = ast.InfixOperator.fromTokenType(infix_token.token_type),
            .right = right_expr,
        },
    };
}

fn parseBooleanLiteral(self: *Self) !?ast.Expression {
    return ast.Expression{
        .boolean_literal = ast.BooleanLiteralExpression{
            .token = self.cur_token,
            .value = self.cur_token.token_type == .true,
        },
    };
}

fn parseIfExpression(self: *Self) !?ast.Expression {
    const if_token = self.cur_token;

    if (!self.expectPeek(.lparen)) {
        return null;
    }
    self.nextToken();

    const condition = try self.alloc.create(ast.Expression);
    condition.* = try self.parseExpression(.lowest) orelse return null;

    if (!self.expectPeek(.rparen)) {
        return null;
    }

    if (!self.expectPeek(.lbrace)) {
        return null;
    }

    const consequence = try self.alloc.create(ast.BlockStatement);
    consequence.* = try self.parseBlockStatement() orelse return null;

    var alternative: ?*ast.BlockStatement = null;
    if (self.peekTokenIs(.@"else")) {
        self.nextToken();

        if (!self.expectPeek(.lbrace)) {
            return null;
        }

        alternative = try self.alloc.create(ast.BlockStatement);
        alternative.?.* = try self.parseBlockStatement() orelse return null;
    }

    return ast.Expression{
        .@"if" = ast.IfExpression{
            .token = if_token,
            .condition = condition,
            .consequence = consequence,
            .alternative = alternative,
        },
    };
}

fn parseFunctionLiteral(self: *Self) !?ast.Expression {
    const fn_token = self.cur_token;

    if (!self.expectPeek(.lparen)) return null;

    const parameters = try self.parseFunctionParameters() orelse return null;

    if (!self.expectPeek(.lbrace)) {
        return null;
    }

    const body = try self.alloc.create(ast.BlockStatement);
    body.* = try self.parseBlockStatement() orelse return null;

    return ast.Expression{
        .function_literal = ast.FunctionLiteralExpression{
            .token = fn_token,
            .parameters = parameters,
            .body = body,
        },
    };
}

fn parseFunctionParameters(self: *Self) !?[]ast.Identifier {
    var params = std.ArrayList(ast.Identifier).init(self.alloc);
    errdefer params.deinit();

    if (self.peekTokenIs(.rparen)) {
        self.nextToken();
        return try params.toOwnedSlice();
    }

    self.nextToken();

    const first_param = ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    };
    try params.append(first_param);

    while (self.peekTokenIs(.comma)) {
        self.nextToken();
        self.nextToken();

        const param = ast.Identifier{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        };
        try params.append(param);
    }

    if (!self.expectPeek(.rparen)) {
        return null;
    }

    return try params.toOwnedSlice();
}

fn parseCallExpression(self: *Self, left: ast.Expression) !?ast.Expression {
    const call_token = self.cur_token;

    const function = try self.alloc.create(ast.Expression);
    function.* = left;

    const args = try self.parseCallArguments() orelse return null;

    return ast.Expression{ .call = ast.CallExpression{
        .token = call_token,
        .function = function,
        .arguments = args,
    } };
}

fn parseCallArguments(self: *Self) !?[]ast.Expression {
    var args = std.ArrayList(ast.Expression).init(self.alloc);
    errdefer args.deinit();

    if (self.peekTokenIs(.rparen)) {
        self.nextToken();
        return try args.toOwnedSlice();
    }

    self.nextToken();

    const first_arg = try self.parseExpression(.lowest) orelse return null;
    try args.append(first_arg);

    while (self.peekTokenIs(.comma)) {
        self.nextToken();
        self.nextToken();

        const arg = try self.parseExpression(.lowest) orelse return null;
        try args.append(arg);
    }

    if (!self.expectPeek(.rparen)) {
        return null;
    }

    return try args.toOwnedSlice();
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
        self.alloc,
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
            .lt, .lte, .gt, .gte => .lessGreater,
            .plus, .minus => .sum,
            .asterisk, .slash => .product,
            .lparen => .call,
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

fn getPrefixParseFn(token_type: Token.Type) ?*const PrefixParseFn {
    return switch (token_type) {
        .ident => parseIdentifier,
        .int => parseIntegerLiteral,
        .minus => parsePrefixExpression,
        .bang => parsePrefixExpression,
        .true => parseBooleanLiteral,
        .false => parseBooleanLiteral,
        .lparen => parseGroupedExpression,
        .@"if" => parseIfExpression,
        .function => parseFunctionLiteral,
        else => null,
    };
}

fn getInfixParseFn(token_type: Token.Type) ?*const InfixParseFn {
    return switch (token_type) {
        .plus => parseInfixExpression,
        .minus => parseInfixExpression,
        .asterisk => parseInfixExpression,
        .slash => parseInfixExpression,
        .lt => parseInfixExpression,
        .lte => parseInfixExpression,
        .gt => parseInfixExpression,
        .gte => parseInfixExpression,
        .eq => parseInfixExpression,
        .not_eq => parseInfixExpression,
        .lparen => parseCallExpression,
        else => null,
    };
}

test "let statements" {
    const tests = [_]struct {
        input: []const u8,
        expected_identifier: []const u8,
        expected_value: i64,
    }{
        .{ .input = "let x = 5", .expected_identifier = "x", .expected_value = 5 },
        .{ .input = "let y = 10", .expected_identifier = "y", .expected_value = 10 },
        .{ .input = "let foobar = 838383", .expected_identifier = "foobar", .expected_value = 838383 },
    };

    const allocator = testing.allocator;

    for (tests) |t| {
        var ast_arena = std.heap.ArenaAllocator.init(allocator);
        defer ast_arena.deinit();

        var lexer = Lexer.init(t.input);

        const parser_alloc = ast_arena.allocator();
        var p = init(parser_alloc, &lexer);

        const program = try p.parseProgram();
        defer p.deinit();

        if (p.getErrors().len > 0) {
            for (p.getErrors()) |err| {
                std.debug.print("parser error: {s}\n", .{err.toString()});
            }
        }

        try testing.expectEqual(1, program.statements.len);

        const let_stmt = program.statements[0].let;
        try testIdentifier(let_stmt.name, t.expected_identifier);
        try testLiteralExpression(let_stmt.value, t.expected_value);

        std.debug.print("program:\n{s}\n", .{program.toString()});
    }
}

test "return statements" {
    const allocator = testing.allocator;

    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();

    const input =
        \\return 5
        \\return 10
        \\return 993322
    ;
    var lexer = Lexer.init(input);

    const parser_alloc = ast_arena.allocator();
    var p = init(parser_alloc, &lexer);

    const program = try p.parseProgram();
    defer p.deinit();

    try testing.expectEqual(3, program.statements.len);

    std.debug.print("program:\n{s}\n", .{program.toString()});
}

test "identifier expression" {
    const allocator = testing.allocator;

    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();

    const input = "foobar;";
    var lexer = Lexer.init(input);

    const parser_alloc = ast_arena.allocator();
    var p = init(parser_alloc, &lexer);

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

    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();

    const input = "5;";
    var lexer = Lexer.init(input);

    const parser_alloc = ast_arena.allocator();
    var p = init(parser_alloc, &lexer);

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
        var ast_arena = std.heap.ArenaAllocator.init(allocator);
        defer ast_arena.deinit();

        var lexer = Lexer.init(t.input);

        const parser_alloc = ast_arena.allocator();
        var p = init(parser_alloc, &lexer);

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
        var ast_arena = std.heap.ArenaAllocator.init(allocator);
        defer ast_arena.deinit();

        var lexer = Lexer.init(t.input);

        const parser_alloc = ast_arena.allocator();
        var p = init(parser_alloc, &lexer);

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

    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();

    const input = "true; false;";
    var lexer = Lexer.init(input);

    const parser_alloc = ast_arena.allocator();
    var p = init(parser_alloc, &lexer);

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
        try testLiteralExpression(stmt.expression.expression, value);
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
        .{ .input = "a + add(b * c) + d;", .expected = "((a + add((b * c))) + d)" },
        .{ .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
    };

    const allocator = testing.allocator;

    for (tests) |t| {
        var ast_arena = std.heap.ArenaAllocator.init(allocator);
        defer ast_arena.deinit();

        var lexer = Lexer.init(t.input);

        const parser_alloc = ast_arena.allocator();
        var p = init(parser_alloc, &lexer);

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

test "if expression" {
    const allocator = testing.allocator;

    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();

    const input = "if (x < y) { x }";
    var lexer = Lexer.init(input);

    const parser_alloc = ast_arena.allocator();
    var p = init(parser_alloc, &lexer);

    const program = try p.parseProgram();
    defer p.deinit();

    if (p.getErrors().len > 0) {
        for (p.getErrors()) |err| {
            std.debug.print("parser error: {s}\n", .{err.toString()});
        }
    }

    try testing.expectEqual(1, program.statements.len);

    const stmt = program.statements[0];
    try testInfixExpression(stmt.expression.expression.@"if".condition.*, "x", ast.InfixOperator.lt, "y");
    try testing.expectEqual(1, stmt.expression.expression.@"if".consequence.statements.len);

    const consequence_stmt = stmt.expression.expression.@"if".consequence.statements[0];
    try testLiteralExpression(consequence_stmt.expression.expression, "x");

    try testing.expectEqual(null, stmt.expression.expression.@"if".alternative);

    std.debug.print("program:\n{s}\n", .{program.toString()});
}

test "if else expression" {
    const allocator = testing.allocator;

    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();

    const input = "if (x < y) { x } else { y }";
    var lexer = Lexer.init(input);

    const parser_alloc = ast_arena.allocator();
    var p = init(parser_alloc, &lexer);

    const program = try p.parseProgram();
    defer p.deinit();

    if (p.getErrors().len > 0) {
        for (p.getErrors()) |err| {
            std.debug.print("parser error: {s}\n", .{err.toString()});
        }
    }

    try testing.expectEqual(1, program.statements.len);

    const stmt = program.statements[0];
    try testInfixExpression(stmt.expression.expression.@"if".condition.*, "x", ast.InfixOperator.lt, "y");
    try testing.expectEqual(1, stmt.expression.expression.@"if".consequence.statements.len);

    const consequence_stmt = stmt.expression.expression.@"if".consequence.statements[0];
    try testLiteralExpression(consequence_stmt.expression.expression, "x");

    try testing.expectEqual(1, stmt.expression.expression.@"if".alternative.?.statements.len);

    const alternative_stmt = stmt.expression.expression.@"if".alternative.?.statements[0];
    try testLiteralExpression(alternative_stmt.expression.expression, "y");

    std.debug.print("program:\n{s}\n", .{program.toString()});
}

test "function literal expression" {
    const allocator = testing.allocator;
    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();

    const input = "fn(x, y) { x + y; }";
    var lexer = Lexer.init(input);

    const parser_alloc = ast_arena.allocator();
    var p = init(parser_alloc, &lexer);

    const program = try p.parseProgram();
    defer p.deinit();

    if (p.getErrors().len > 0) {
        for (p.getErrors()) |err| {
            std.debug.print("parser error: {s}\n", .{err.toString()});
        }
    }

    try testing.expectEqual(1, program.statements.len);

    const fn_lit = program.statements[0].expression.expression.function_literal;
    try testing.expectEqual(2, fn_lit.parameters.len);
    try testIdentifier(fn_lit.parameters[0], "x");
    try testIdentifier(fn_lit.parameters[1], "y");

    try testing.expectEqual(1, fn_lit.body.statements.len);
    const body_stmt = fn_lit.body.statements[0];
    try testInfixExpression(body_stmt.expression.expression, "x", ast.InfixOperator.plus, "y");

    std.debug.print("program:\n{s}\n", .{program.toString()});
}

test "function parameters" {
    const tests = [_]struct {
        input: []const u8,
        expected_params: []const []const u8,
    }{
        .{ .input = "fn() {};", .expected_params = &.{} },
        .{ .input = "fn(x) {};", .expected_params = &.{"x"} },
        .{ .input = "fn(x, y, z) {};", .expected_params = &.{ "x", "y", "z" } },
    };

    const allocator = testing.allocator;

    for (tests) |t| {
        var ast_arena = std.heap.ArenaAllocator.init(allocator);
        defer ast_arena.deinit();

        var lexer = Lexer.init(t.input);

        const parser_alloc = ast_arena.allocator();
        var p = init(parser_alloc, &lexer);

        const program = try p.parseProgram();
        defer p.deinit();

        if (p.getErrors().len > 0) {
            for (p.getErrors()) |err| {
                std.debug.print("parser error: {s}\n", .{err.toString()});
            }
        }

        try testing.expectEqual(1, program.statements.len);

        const fn_lit = program.statements[0].expression.expression.function_literal;
        try testing.expectEqual(t.expected_params.len, fn_lit.parameters.len);

        for (t.expected_params, 0..) |param, i| {
            try testIdentifier(fn_lit.parameters[i], param);
        }

        std.debug.print("program:\n{s}\n", .{program.toString()});
    }
}

test "call expression" {
    const allocator = testing.allocator;

    var ast_arena = std.heap.ArenaAllocator.init(allocator);
    defer ast_arena.deinit();

    const input = "add(1, 2 * 3, 4 + 5);";
    var lexer = Lexer.init(input);

    const parser_alloc = ast_arena.allocator();
    var p = init(parser_alloc, &lexer);

    const program = try p.parseProgram();
    defer p.deinit();

    if (p.getErrors().len > 0) {
        for (p.getErrors()) |err| {
            std.debug.print("parser error: {s}\n", .{err.toString()});
        }
    }

    try testing.expectEqual(1, program.statements.len);

    const call_expr = program.statements[0].expression.expression.call;
    try testIdentifier(call_expr.function.identifier, "add");
    try testing.expectEqual(3, call_expr.arguments.len);
    try testLiteralExpression(call_expr.arguments[0], 1);
    try testInfixExpression(call_expr.arguments[1], 2, ast.InfixOperator.asterisk, 3);
    try testInfixExpression(call_expr.arguments[2], 4, ast.InfixOperator.plus, 5);

    std.debug.print("program:\n{s}\n", .{program.toString()});
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
    const T = comptime @TypeOf(expected);

    switch (@typeInfo(T)) {
        .int, .comptime_int => try testIntegerLiteral(expr, expected),
        .pointer => |ptr| {
            if (@typeInfo(ptr.child).array.child != u8) unreachable;
            try testIdentifier(expr.identifier, expected);
        },
        .bool => try testBooleanLiteral(expr, expected),
        else => |ti| {
            std.debug.print("Unsupported type: {any}\n", .{ti});
        },
    }
}

fn testIntegerLiteral(expr: ast.Expression, value: i64) !void {
    const int_lit = expr.integer_literal;
    try testing.expectEqual(.int, int_lit.token.token_type);
    try testing.expectEqual(value, int_lit.value);
}

fn testIdentifier(ident: ast.Identifier, value: []const u8) !void {
    try testing.expectEqual(.ident, ident.token.token_type);
    try testing.expectEqualStrings(value, ident.value);
}

fn testBooleanLiteral(expr: ast.Expression, value: bool) !void {
    const bool_lit = expr.boolean_literal;
    try testing.expect(bool_lit.token.token_type == .true or bool_lit.token.token_type == .false);
    try testing.expectEqual(value, bool_lit.value);
}
