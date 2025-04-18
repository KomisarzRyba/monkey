const std = @import("std");

pub const Type = enum {
    illegal,
    eof,

    // Identifiers + literals
    ident,
    int,

    // Operators
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    lt,
    gt,
    eq,
    not_eq,

    // Delimiters
    comma,
    semicolon,

    lparen,
    rparen,
    lbrace,
    rbrace,

    // Keywords
    function,
    let,
    true,
    false,
    @"if",
    @"else",
    @"return",

    pub fn name(self: Type) []const u8 {
        return switch (self) {
            .illegal => "ILLEGAL",
            .eof => "EOF",
            .ident => "IDENT",
            .int => "INT",
            .assign => "=",
            .plus => "+",
            .minus => "-",
            .bang => "!",
            .asterisk => "*",
            .slash => "/",
            .lt => "<",
            .gt => ">",
            .eq => "==",
            .not_eq => "!=",
            .comma => ",",
            .semicolon => ";",
            .lparen => "(",
            .rparen => ")",
            .lbrace => "{",
            .rbrace => "}",
            .function => "FUNCTION",
            .let => "LET",
            .true => "TRUE",
            .false => "FALSE",
            .@"if" => "IF",
            .@"else" => "ELSE",
            .@"return" => "RETURN",
        };
    }
};

pub const Location = struct {
    line: usize,
    column: usize,

    pub fn toString(self: Location) []const u8 {
        return std.fmt.allocPrint(
            std.heap.page_allocator,
            "{d}:{d}",
            .{ self.line, self.column },
        ) catch unreachable;
    }
};

token_type: Type,
literal: []const u8,
location: Location,

const keyword_map = std.StaticStringMap(Type).initComptime(.{
    .{ "fn", .function },
    .{ "let", .let },
    .{ "true", .true },
    .{ "false", .false },
    .{ "if", .@"if" },
    .{ "else", .@"else" },
    .{ "return", .@"return" },
});

pub fn keyword(ident: []const u8) ?Type {
    return keyword_map.get(ident);
}
