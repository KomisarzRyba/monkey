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
        '<' => .lt,
        '>' => .gt,

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
    ;

    const expectedTokenTypes = [_]Token.Type{
        .let,
        .ident,
        .assign,
        .int,
        .semicolon,
        .let,
        .ident,
        .assign,
        .int,
        .semicolon,
        .let,
        .ident,
        .assign,
        .function,
        .lparen,
        .ident,
        .comma,
        .ident,
        .rparen,
        .lbrace,
        .ident,
        .plus,
        .ident,
        .semicolon,
        .rbrace,
        .semicolon,
        .let,
        .ident,
        .assign,
        .ident,
        .lparen,
        .ident,
        .comma,
        .ident,
        .rparen,
        .semicolon,
        .bang,
        .minus,
        .slash,
        .asterisk,
        .int,
        .semicolon,
        .int,
        .lt,
        .int,
        .gt,
        .int,
        .semicolon,
        .@"if",
        .lparen,
        .int,
        .lt,
        .int,
        .rparen,
        .lbrace,
        .@"return",
        .true,
        .semicolon,
        .rbrace,
        .@"else",
        .lbrace,
        .@"return",
        .false,
        .semicolon,
        .rbrace,
        .int,
        .eq,
        .int,
        .semicolon,
        .int,
        .not_eq,
        .int,
        .semicolon,
    };

    var lexer = init(input);

    for (expectedTokenTypes) |expected| {
        const actual = lexer.next().token_type;
        try testing.expectEqualDeep(expected, actual);
    }
}
