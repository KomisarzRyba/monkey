const std = @import("std");
const testing = std.testing;

const Token = @import("token.zig").Token;

pub const Lexer = struct {
    input: []const u8,
    pos: usize,
    read_pos: usize,
    ch: u8,

    const Self = @This();

    pub fn init(input: []const u8) Self {
        var lexer = Self{
            .input = input,
            .pos = 0,
            .read_pos = 0,
            .ch = 0,
        };
        lexer.readChar();
        return lexer;
    }

    fn hasNextChar(self: *Self) bool {
        return self.read_pos < self.input.len;
    }

    fn readChar(self: *Self) void {
        if (!self.hasNextChar()) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_pos];
        }
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

    fn nextToken(self: *Self) Token {
        self.skipWhitespace();

        const tok: Token = switch (self.ch) {
            0 => .eof,

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
                    return kw_token;
                }
                return .{ .ident = ident };
            },
            '0'...'9' => {
                return .{ .int = self.readNumber() };
            },

            else => .illegal,
        };

        self.readChar();
        return tok;
    }
};

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

    const expectedTokens = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .semicolon,
        .let,
        .{ .ident = "ten" },
        .assign,
        .{ .int = "10" },
        .semicolon,
        .let,
        .{ .ident = "add" },
        .assign,
        .function,
        .lparen,
        .{ .ident = "x" },
        .comma,
        .{ .ident = "y" },
        .rparen,
        .lbrace,
        .{ .ident = "x" },
        .plus,
        .{ .ident = "y" },
        .semicolon,
        .rbrace,
        .semicolon,
        .let,
        .{ .ident = "result" },
        .assign,
        .{ .ident = "add" },
        .lparen,
        .{ .ident = "five" },
        .comma,
        .{ .ident = "ten" },
        .rparen,
        .semicolon,
        .bang,
        .minus,
        .slash,
        .asterisk,
        .{ .int = "5" },
        .semicolon,
        .{ .int = "5" },
        .lt,
        .{ .int = "10" },
        .gt,
        .{ .int = "5" },
        .semicolon,
        .@"if",
        .lparen,
        .{ .int = "5" },
        .lt,
        .{ .int = "10" },
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
        .{ .int = "10" },
        .eq,
        .{ .int = "10" },
        .semicolon,
        .{ .int = "10" },
        .not_eq,
        .{ .int = "9" },
        .semicolon,
    };

    var lexer = Lexer.init(input);

    for (expectedTokens) |expected| {
        const actual = lexer.nextToken();
        try testing.expectEqualDeep(expected, actual);
    }
}
