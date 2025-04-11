const std = @import("std");

pub const Token = union(enum) {
    illegal,
    eof,

    // Identifiers + literals
    ident: []const u8,
    int: []const u8,

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

    pub fn keyword(ident: []const u8) ?Token {
        const keyword_map = std.StaticStringMap(Token).initComptime(.{
            .{ "fn", .function },
            .{ "let", .let },
            .{ "true", .true },
            .{ "false", .false },
            .{ "if", .@"if" },
            .{ "else", .@"else" },
            .{ "return", .@"return" },
        });
        return keyword_map.get(ident);
    }
};
