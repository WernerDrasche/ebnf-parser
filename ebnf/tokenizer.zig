const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const State = enum { terminal, non_terminal, outer };
pub const Token = struct {
    state: State,
    content: []const u8,
};

pub fn tokenize(contents: []const u8, allocator: Allocator) ![]Token {
    var tokens = ArrayList(Token).init(allocator);
    var token_start: usize = 0;
    var token_end: usize = 0;
    var state = State.outer;
    for (contents) |c| {
        switch (state) {
            .terminal => {
                if (c == '\"') {
                    try tokens.append(.{ .state = state, .content = contents[token_start..token_end] });
                    state = .outer;
                    token_start = token_end + 1;
                }
                token_end += 1;
            },
            .non_terminal => {
                if (c == ' ' or c == ';' or c == '\n') {
                    try tokens.append(.{ .state = state, .content = contents[token_start..token_end] });
                    state = .outer;
                    token_start = token_end + 1;
                }
                token_end += 1;
            },
            .outer => {
                if (c == ' ' or c == '\n') {
                    token_start += 1;
                    token_end += 1;
                } else if (c == '\"') {
                    state = .terminal;
                    token_start += 1;
                    token_end += 1;
                } else {
                    state = .non_terminal;
                    token_end += 1;
                }
            },
        }
    }
    const last = Token{ .state = state, .content = contents[token_start..token_end] };
    if (last.content.len != 0) try tokens.append(last);
    return tokens.toOwnedSlice();
}
