const std = @import("std");
const testing = std.testing;

pub const Token = @import("token.zig");

input: []const u8,
pos: usize = 0,
read_pos: usize = 0,
ch: u8 = 0,
location: Token.Location = .{ .line = 1, .column = 0 },

const Self = @This();

pub fn init(input: []const u8) Self {
    var lexer = Self{
        .input = input,
    };
    lexer.readChar();
    return lexer;
}

pub fn next(self: *Self) Token {
    self.skipWhitespace();

    const start_pos = self.pos;
    const start_loc = self.location;
    const token_type: Token.Type = switch (self.ch) {
        0 => return .{
            .token_type = .eof,
            .literal = "",
            .location = start_loc,
        },

        '=' => peek: {
            if (self.peekChar() == '=') {
                self.readChar();
                break :peek .eq;
            }
            break :peek .assign;
        },
        '+' => .plus,
        '-' => .minus,
        '!' => peek: {
            if (self.peekChar() == '=') {
                self.readChar();
                break :peek .not_eq;
            }
            break :peek .bang;
        },
        '*' => .asterisk,
        '/' => .slash,
        '<' => peek: {
            if (self.peekChar() == '=') {
                self.readChar();
                break :peek .lte;
            }
            break :peek .lt;
        },
        '>' => peek: {
            if (self.peekChar() == '=') {
                self.readChar();
                break :peek .gte;
            }
            break :peek .gt;
        },

        ',' => .comma,
        ';' => .semicolon,

        '(' => .lparen,
        ')' => .rparen,
        '{' => .lbrace,
        '}' => .rbrace,

        'a'...'z', 'A'...'Z' => {
            const ident = self.readIdent();
            if (Token.keyword(ident)) |kw_token| {
                return .{
                    .token_type = kw_token,
                    .literal = ident,
                    .location = start_loc,
                };
            }
            return .{
                .token_type = .ident,
                .literal = ident,
                .location = start_loc,
            };
        },

        '0'...'9' => {
            return .{
                .token_type = .int,
                .literal = self.readNumber(),
                .location = start_loc,
            };
        },

        '"' => {
            const string = self.readString();
            return .{
                .token_type = .string,
                .literal = string,
                .location = start_loc,
            };
        },

        else => .illegal,
    };

    self.readChar();
    return .{
        .token_type = token_type,
        .literal = self.input[start_pos..self.pos],
        .location = self.location,
    };
}

fn hasNextChar(self: *Self) bool {
    return self.read_pos < self.input.len;
}

fn readChar(self: *Self) void {
    if (!self.hasNextChar()) {
        self.pos = self.read_pos;
        self.read_pos += 1;
        self.ch = 0;
        return;
    }
    if (self.ch == '\n') {
        self.location.line += 1;
        self.location.column = 0;
    } else {
        self.location.column += 1;
    }

    self.ch = self.input[self.read_pos];
    self.pos = self.read_pos;
    self.read_pos += 1;
}

fn peekChar(self: *Self) u8 {
    if (!self.hasNextChar()) {
        return 0;
    }
    return self.input[self.read_pos];
}

fn readIdent(self: *Self) []const u8 {
    const start_pos = self.pos;
    while (std.ascii.isAlphabetic(self.ch) or self.ch == '_') {
        self.readChar();
    }
    return self.input[start_pos..self.pos];
}

fn readNumber(self: *Self) []const u8 {
    const start_pos = self.pos;
    while (std.ascii.isDigit(self.ch)) {
        self.readChar();
    }
    return self.input[start_pos..self.pos];
}

fn readString(self: *Self) []const u8 {
    self.readChar();
    const start_pos = self.pos;
    while (self.hasNextChar() and self.ch != '"') {
        self.readChar();
    }
    if (self.hasNextChar()) {
        self.readChar();
        return self.input[start_pos .. self.pos - 1];
    }
    return self.input[start_pos..self.pos];
}

fn skipWhitespace(self: *Self) void {
    while (std.ascii.isWhitespace(self.ch)) {
        self.readChar();
    }
}

test "monkey" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\x + y;
        \\};
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\if (5 < 10) {
        \\return true;
        \\} else {
        \\return false;
        \\}
        \\10 == 10;
        \\10 != 9;
        \\"foobar"
        \\"foo bar"
    ;

    const tests = [_]struct {
        token_type: Token.Type,
        literal: []const u8,
    }{
        .{ .token_type = .let, .literal = "let" },
        .{ .token_type = .ident, .literal = "five" },
        .{ .token_type = .assign, .literal = "=" },
        .{ .token_type = .int, .literal = "5" },
        .{ .token_type = .semicolon, .literal = ";" },
        .{ .token_type = .let, .literal = "let" },
        .{ .token_type = .ident, .literal = "ten" },
        .{ .token_type = .assign, .literal = "=" },
        .{ .token_type = .int, .literal = "10" },
        .{ .token_type = .semicolon, .literal = ";" },
        .{ .token_type = .let, .literal = "let" },
        .{ .token_type = .ident, .literal = "add" },
        .{ .token_type = .assign, .literal = "=" },
        .{ .token_type = .function, .literal = "fn" },
        .{ .token_type = .lparen, .literal = "(" },
        .{ .token_type = .ident, .literal = "x" },
        .{ .token_type = .comma, .literal = "," },
        .{ .token_type = .ident, .literal = "y" },
        .{ .token_type = .rparen, .literal = ")" },
        .{ .token_type = .lbrace, .literal = "{" },
        .{ .token_type = .ident, .literal = "x" },
        .{ .token_type = .plus, .literal = "+" },
        .{ .token_type = .ident, .literal = "y" },
        .{ .token_type = .semicolon, .literal = ";" },
        .{ .token_type = .rbrace, .literal = "}" },
        .{ .token_type = .semicolon, .literal = ";" },
        .{ .token_type = .let, .literal = "let" },
        .{ .token_type = .ident, .literal = "result" },
        .{ .token_type = .assign, .literal = "=" },
        .{ .token_type = .ident, .literal = "add" },
        .{ .token_type = .lparen, .literal = "(" },
        .{ .token_type = .ident, .literal = "five" },
        .{ .token_type = .comma, .literal = "," },
        .{ .token_type = .ident, .literal = "ten" },
        .{ .token_type = .rparen, .literal = ")" },
        .{ .token_type = .semicolon, .literal = ";" },
        .{ .token_type = .bang, .literal = "!" },
        .{ .token_type = .minus, .literal = "-" },
        .{ .token_type = .slash, .literal = "/" },
        .{ .token_type = .asterisk, .literal = "*" },
        .{ .token_type = .int, .literal = "5" },
        .{ .token_type = .semicolon, .literal = ";" },
        .{ .token_type = .int, .literal = "5" },
        .{ .token_type = .lt, .literal = "<" },
        .{ .token_type = .int, .literal = "10" },
        .{ .token_type = .gt, .literal = ">" },
        .{ .token_type = .int, .literal = "5" },
        .{ .token_type = .semicolon, .literal = ";" },
        .{ .token_type = .@"if", .literal = "if" },
        .{ .token_type = .lparen, .literal = "(" },
        .{ .token_type = .int, .literal = "5" },
        .{ .token_type = .lt, .literal = "<" },
        .{ .token_type = .int, .literal = "10" },
        .{ .token_type = .rparen, .literal = ")" },
        .{ .token_type = .lbrace, .literal = "{" },
        .{ .token_type = .@"return", .literal = "return" },
        .{ .token_type = .true, .literal = "true" },
        .{ .token_type = .semicolon, .literal = ";" },
        .{ .token_type = .rbrace, .literal = "}" },
        .{ .token_type = .@"else", .literal = "else" },
        .{ .token_type = .lbrace, .literal = "{" },
        .{ .token_type = .@"return", .literal = "return" },
        .{ .token_type = .false, .literal = "false" },
        .{ .token_type = .semicolon, .literal = ";" },
        .{ .token_type = .rbrace, .literal = "}" },
        .{ .token_type = .int, .literal = "10" },
        .{ .token_type = .eq, .literal = "==" },
        .{ .token_type = .int, .literal = "10" },
        .{ .token_type = .semicolon, .literal = ";" },
        .{ .token_type = .int, .literal = "10" },
        .{ .token_type = .not_eq, .literal = "!=" },
        .{ .token_type = .int, .literal = "9" },
        .{ .token_type = .semicolon, .literal = ";" },
        .{ .token_type = .string, .literal = "foobar" },
        .{ .token_type = .string, .literal = "foo bar" },
    };

    var lexer = init(input);

    for (tests) |t| {
        const actual = lexer.next();
        try testing.expectEqual(t.token_type, actual.token_type);
        try testing.expectEqualStrings(t.literal, actual.literal);
    }
}
