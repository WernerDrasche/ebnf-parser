const std = @import("std");
const mem = std.mem;
const dbg = std.debug;
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

const INVALID_QUERY_ARG = "invalid query argument";

const Range = struct {
    from: u8,
    to: u8, // inclusive
};

const ElementTag = enum {
    terminal,
    non_terminal,
    sequence,
    alternative,
    repeat,
    option,
    range,
};

const Element = union(ElementTag) {
    terminal: []const u8,
    non_terminal: []const u8,
    sequence: []Element,
    alternative: []Element,
    repeat: *Element,
    option: *Element,
    range: Range,

    fn create(tokens: []Token, target: ElementTag, allocator: Allocator) !Element {
        switch (target) {
            .alternative, .sequence => {
                const result = try classifyTokens(tokens, allocator);
                const sections = result.classes;
                defer sections.deinit();
                if (sections.items.len == 1) {
                    const section = sections.items[0];
                    return try create(section.tokens, section.inside, allocator);
                }
                const elements = try allocator.alloc(Element, sections.items.len);
                for (sections.items, 0..) |seq, i| {
                    elements[i] = try create(seq.tokens, seq.inside, allocator);
                }
                return if (result.from == .alternative) .{ .alternative = elements } else .{ .sequence = elements };
            },
            .option => {
                var element = try allocator.create(Element);
                element.* = try create(tokens, .alternative, allocator);
                return Element{ .option = element };
            },
            .repeat => {
                var element = try allocator.create(Element);
                element.* = try create(tokens, .alternative, allocator);
                return Element{ .repeat = element };
            },
            .range => {
                //only ascii
                const from: u8 = tokens[0].content[0];
                const to: u8 = tokens[2].content[0];
                return Element{ .range = .{ .from = from, .to = to } };
            },
            .terminal => return Element{ .terminal = tokens[0].content },
            .non_terminal => return Element{ .non_terminal = tokens[0].content },
        }
        unreachable;
    }

    fn free(self: Element, allocator: Allocator) void {
        switch (self) {
            .sequence, .alternative => |elements| {
                for (elements) |element| {
                    element.free(allocator);
                }
                allocator.free(elements);
            },
            .repeat, .option => |element| {
                element.free(allocator);
                allocator.destroy(element);
            },
            else => return,
        }
    }

    pub fn format(self: Element, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s}( ", .{@tagName(self)});
        switch (self) {
            .terminal => |terminal| try writer.print("\"{s}\" ", .{terminal}),
            .non_terminal => |name| try writer.print("{s} ", .{name}),
            .sequence, .alternative => |elements| {
                for (elements) |element| {
                    try writer.print("{} ", .{element});
                }
            },
            .repeat, .option => |element| try writer.print("{} ", .{element.*}),
            .range => |range| {
                try writer.print("{c} ... {c} ", .{ range.from, range.to });
            },
        }
        try writer.print(") ", .{});
    }
};

pub const Rule = struct {
    name: []const u8,
    definition: Element,
    is_tmp: bool,
    generated_by: ?usize,
    canonical: usize,

    pub fn display(self: Rule, writer: anytype) !void {
        const eq = if (self.is_tmp) "=" else ":=";
        try writer.print("{s} {s} {}\n", .{ self.name, eq, self.definition });
    }
};

pub const Rules = struct {
    tokens: []Token,
    rules: ArrayList(Rule),
    names: StringHashMap(usize),
    allocator: Allocator,

    ///grammar must live as long as Rules
    pub fn create(grammar: []const u8, allocator: Allocator) !Rules {
        const tokens = try tokenizer.tokenize(grammar, allocator);
        var rules = Rules{
            .tokens = tokens,
            .rules = ArrayList(Rule).init(allocator),
            .names = StringHashMap(usize).init(allocator),
            .allocator = allocator,
        };
        var start: usize = 0;
        while (nextOccurence(";", tokens, start)) |end| {
            const name = tokens[start].content;
            const eq = tokens[start + 1].content;
            // zig fmt: off
            const is_tmp = 
                if (mem.eql(u8, eq, ":=")) false 
                else if (mem.eql(u8, eq, "=")) true
                else return error.InvalidEq;
            // zig fmt: on
            const def = tokens[start + 2 .. end];
            const element = try Element.create(def, .alternative, allocator);
            const idx = rules.rules.items.len;
            try rules.names.putNoClobber(name, idx);
            try rules.rules.append(.{
                .name = name,
                .definition = element,
                .is_tmp = is_tmp,
                .generated_by = null,
                .canonical = idx,
            });
            start = end + 1;
        }
        try rules.handleCanonical();
        try rules.removeBasicLeftRecursion();
        return rules;
    }

    fn handleCanonical(self: *Rules) !void {
        var new_rules = ArrayList(Rule).init(self.allocator);
        var canonical = self.rules.items.len;
        for (self.rules.items) |*rule| {
            if (mem.indexOf(u8, rule.name, "'")) |idx| {
                const target = rule.name[0..idx];
                const result = try self.names.getOrPut(target);
                rule.canonical = if (result.found_existing) result.value_ptr.* else blk: {
                    result.value_ptr.* = canonical;
                    try new_rules.append(.{
                        .name = target,
                        .is_tmp = rule.is_tmp,
                        .generated_by = null,
                        .canonical = canonical,
                        .definition = .{ .terminal = "" },
                    });
                    const old = canonical;
                    canonical += 1;
                    break :blk old;
                };
            }
        }
        try self.rules.appendSlice(try new_rules.toOwnedSlice());
    }

    ///this flattens the syntax tree
    pub fn parse(self: Rules, program: []const u8, rule: usize) !Node {
        const definition = self.rules.items[rule].definition;
        var node = try Node.parse(program, definition, self, self.allocator);
        const consumed = node.consumed;
        const branches = try node.flatten(self, self.allocator);
        for (branches) |*n| {
            //TODO: flatten or canonicalize creates consumed off by one error
            n.* = n.canonicalize(self, self.allocator);
        }
        const application = Application{ .rule = rule, .branches = branches };
        return .{ .consumed = consumed, .t = .{ .application = application } };
    }

    ///only supports very basic left recursion like expr = expr + expr
    fn removeBasicLeftRecursion(self: *Rules) !void {
        var generated_rules = ArrayList(Rule).init(self.allocator);
        defer generated_rules.deinit();
        var gen_i = self.rules.items.len;
        for (self.rules.items, 0..) |*rule, i| {
            const name = rule.name;
            const definition = rule.definition;
            if (isLeftRecursive(name, definition)) {
                const rr_name = try self.allocator.alloc(u8, name.len + 1);
                mem.copy(u8, rr_name, name);
                rr_name[name.len] = '\'';
                var rr_elements = ArrayList(Element).init(self.allocator);
                var new_elements = ArrayList(Element).init(self.allocator);
                switch (definition) {
                    .alternative => |alternatives| {
                        for (alternatives) |alternative| {
                            const rr_non_terminal = try self.allocator.create(Element);
                            rr_non_terminal.* = Element{ .non_terminal = rr_name };
                            const option = Element{ .option = rr_non_terminal };
                            if (isLeftRecursive(name, alternative)) {
                                switch (alternative) {
                                    .sequence => |sequence| {
                                        var elements = try ArrayList(Element).initCapacity(self.allocator, sequence.len);
                                        switch (sequence[0]) {
                                            .non_terminal => {
                                                try elements.appendSlice(sequence[1..]);
                                                try elements.append(option);
                                                const rr_element = Element{ .sequence = try elements.toOwnedSlice() };
                                                try rr_elements.append(rr_element);
                                                self.allocator.free(sequence);
                                            },
                                            else => return error.InvalidRule,
                                        }
                                    },
                                    else => return error.InvalidRule,
                                }
                            } else {
                                const elements = try self.allocator.alloc(Element, 2);
                                elements[0] = alternative;
                                elements[1] = option;
                                const new_element = Element{ .sequence = elements };
                                try new_elements.append(new_element);
                            }
                        }
                        self.allocator.free(alternatives);
                    },
                    else => return error.InvalidRule,
                }
                var elements = try rr_elements.toOwnedSlice();
                const rr_alternatives: Element = if (elements.len > 1) .{ .alternative = elements } else blk: {
                    const res = elements[0];
                    self.allocator.free(elements);
                    break :blk res;
                };
                try self.names.putNoClobber(rr_name, gen_i);
                try generated_rules.append(.{
                    .name = rr_name,
                    .definition = rr_alternatives,
                    .is_tmp = rule.is_tmp,
                    .generated_by = i,
                    .canonical = gen_i,
                });
                gen_i += 1;
                elements = try new_elements.toOwnedSlice();
                const new_alternatives: Element = if (elements.len > 1) .{ .alternative = elements } else blk: {
                    const res = elements[0];
                    self.allocator.free(elements);
                    break :blk res;
                };
                rule.definition = new_alternatives;
            }
        }
        try self.rules.appendSlice(generated_rules.items);
    }

    pub fn free(self: *Rules) void {
        for (self.rules.items) |rule| {
            rule.definition.free(self.allocator);
            const name = rule.name;
            if (name[name.len - 1] == '\'') {
                self.allocator.free(name);
            }
        }
        self.rules.deinit();
        self.names.deinit();
        self.allocator.free(self.tokens);
    }

    pub fn display(self: Rules, writer: anytype) !void {
        for (self.rules.items) |rule| {
            try rule.display(writer);
        }
    }

    pub fn displayEnum(self: Rules, writer: anytype, name: []const u8) !void {
        try writer.print("pub const {s} = enum(usize) {{\n", .{name});
        for (self.rules.items, 0..) |rule, i| {
            const idx = self.names.get(rule.name).?;
            if (rule.is_tmp or rule.generated_by != null or idx != rule.canonical) continue;
            try writer.print("    @\"{s}\" = {},\n", .{ rule.name, i });
        }
        try writer.writeAll("};\n");
    }
};

fn isLeftRecursive(rule: []const u8, element: Element) bool {
    return switch (element) {
        .non_terminal => |name| mem.eql(u8, rule, name),
        .sequence => |sequence| isLeftRecursive(rule, sequence[0]),
        .alternative => |alternatives| blk: {
            for (alternatives) |option| {
                if (isLeftRecursive(rule, option)) {
                    break :blk true;
                }
            }
            break :blk false;
        },
        .option, .repeat => |elem| isLeftRecursive(rule, elem.*),
        else => false,
    };
}

fn nextOccurence(pattern: []const u8, tokens: []Token, start: usize) ?usize {
    var i: usize = start;
    while (i < tokens.len) : (i += 1) {
        if (tokens[i].state != .terminal and std.mem.eql(u8, tokens[i].content, pattern)) return i;
    }
    return null;
}

fn skipUntilClosed(comptime c: u8, tokens: []Token, start: usize) ?usize {
    const closed: u8 = switch (c) {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        else => unreachable,
    };
    var i: usize = start;
    while (i < tokens.len) : (i += 1) {
        const token = tokens[i];
        if (token.state == .terminal) continue;
        if (token.content.len != 1) continue;
        const cmp = token.content[0];
        if (cmp == c) {
            i = skipUntilClosed(c, tokens, i + 1).?;
        } else if (cmp == closed) {
            return i;
        }
    }
    return null;
}

const Classification = struct { tokens: []Token, inside: ElementTag };
const ClassificationResult = struct { classes: ArrayList(Classification), from: ElementTag };

fn classifyTokens(tokens: []Token, allocator: Allocator) !ClassificationResult {
    var alternatives = ArrayList(usize).init(allocator);
    defer alternatives.deinit();
    var classes = ArrayList(Classification).init(allocator);
    var i: usize = 0;
    while (i < tokens.len) : (i += 1) {
        const current = tokens[i];
        var to = i;
        if (current.state != .terminal) {
            if (mem.eql(u8, current.content, "(")) {
                to = skipUntilClosed('(', tokens, i + 1).?;
                try classes.append(.{ .tokens = tokens[i + 1 .. to], .inside = .alternative });
            } else if (mem.eql(u8, current.content, "{")) {
                to = skipUntilClosed('{', tokens, i + 1).?;
                try classes.append(.{ .tokens = tokens[i + 1 .. to], .inside = .repeat });
            } else if (mem.eql(u8, current.content, "[")) {
                to = skipUntilClosed('[', tokens, i + 1).?;
                try classes.append(.{ .tokens = tokens[i + 1 .. to], .inside = .option });
            } else if (mem.eql(u8, current.content, "...")) {
                _ = classes.pop();
                to = i + 1;
                try classes.append(.{ .tokens = tokens[i - 1 .. to + 1], .inside = .range });
            } else if (mem.eql(u8, current.content, "|")) {
                try alternatives.append(i);
            } else {
                try classes.append(.{ .tokens = tokens[i .. i + 1], .inside = .non_terminal });
            }
            i = to;
        } else {
            try classes.append(.{ .tokens = tokens[i .. i + 1], .inside = .terminal });
        }
    }
    if (alternatives.items.len != 0) {
        classes.clearAndFree();
        var start: usize = 0;
        for (alternatives.items) |bound| {
            try classes.append(.{ .tokens = tokens[start..bound], .inside = .sequence });
            start = bound + 1;
        }
        try classes.append(.{ .tokens = tokens[start..], .inside = .sequence });
        return .{ .classes = classes, .from = .alternative };
    }
    return .{ .classes = classes, .from = .sequence };
}

fn _skipWhitespace(contents: []const u8, comptime invert: bool) usize {
    var i: usize = 0;
    while (i < contents.len) : (i += 1) {
        const not_whitespace = mem.indexOf(u8, " \n\t", &[_]u8{contents[i]}) == null;
        if (invert and !not_whitespace) break;
        if (!invert and not_whitespace) break;
    }
    return i;
}

pub fn skipWhitespace(contents: []const u8) usize {
    return _skipWhitespace(contents, false);
}

pub fn skipToWhitespace(contents: []const u8) usize {
    return _skipWhitespace(contents, true);
}

//this is a struct in case we want to add position info later
const Terminal = struct {
    content: []const u8,
};

const ParseTerminalResult = struct {
    terminal: Terminal,
    consumed: usize,
};

fn parseTerminal(contents: []const u8, terminal: []const u8) ?ParseTerminalResult {
    var i = skipWhitespace(contents);
    const end = i + terminal.len;
    if (contents.len < end) return null;
    const cmp = contents[i..end];
    if (mem.eql(u8, cmp, terminal)) {
        return .{ .terminal = .{ .content = cmp }, .consumed = end };
    }
    return null;
}

fn parseRange(contents: []const u8, range: Range) ?ParseTerminalResult {
    const i = skipWhitespace(contents);
    const end = i + 1;
    if (contents.len < end) return null;
    const cmp = contents[i];
    if (range.from <= cmp and cmp <= range.to) {
        return .{ .terminal = .{ .content = contents[i..end] }, .consumed = end };
    }
    return null;
}

const Application = struct {
    rule: ?usize = null,
    branches: []Node,
};

pub const Node = struct {
    const Type = union(enum) {
        application: Application,
        terminal: Terminal,
    };

    consumed: usize,
    t: Type,

    fn parse(contents: []const u8, element: Element, rules: Rules, allocator: Allocator) !Node {
        switch (element) {
            .terminal, .range => {
                const parsed = switch (element) {
                    .terminal => |terminal| parseTerminal(contents, terminal),
                    .range => |range| parseRange(contents, range),
                    else => unreachable,
                };
                if (parsed) |result| {
                    return .{ .consumed = result.consumed, .t = .{ .terminal = result.terminal } };
                }
                return error.CouldNotParse;
            },
            .non_terminal => |non_terminal| {
                //dbg.print("trying to parse {s}\n", .{non_terminal});
                const idx: usize = if (rules.names.get(non_terminal)) |idx| idx else {
                    dbg.print("ERROR: rule {s} doesn't exist\n", .{non_terminal});
                    return error.RuleNotFound;
                };
                const rule = rules.rules.items[idx];
                const definition = rule.definition;
                const node = try Node.parse(contents, definition, rules, allocator);
                const branches = try allocator.alloc(Node, 1);
                branches[0] = node;
                //dbg.print("success with parsing {s}\n", .{non_terminal});
                return .{ .consumed = node.consumed, .t = .{ .application = .{ .rule = idx, .branches = branches } } };
            },
            .sequence => |sequence| {
                const branches = try allocator.alloc(Node, sequence.len);
                var start = contents;
                var consumed: usize = 0;
                for (sequence, 0..) |e, i| {
                    const node = Node.parse(start, e, rules, allocator) catch |err| {
                        for (branches[0..i]) |n| {
                            n.free(allocator);
                        }
                        allocator.free(branches);
                        return err;
                    };
                    branches[i] = node;
                    start = start[node.consumed..];
                    consumed += node.consumed;
                }
                return .{ .consumed = consumed, .t = .{ .application = .{ .branches = branches } } };
            },
            .alternative => |alternative| {
                var tmp = ArrayList(Node).init(allocator);
                defer tmp.deinit();
                for (alternative) |e| {
                    const node = Node.parse(contents, e, rules, allocator) catch continue;
                    try tmp.append(node);
                }
                if (tmp.items.len == 0)
                    return error.CouldNotParse;
                var choice = tmp.items[0];
                for (tmp.items[1..]) |n| {
                    if (n.consumed > choice.consumed) {
                        choice.free(allocator);
                        choice = n;
                    } else {
                        n.free(allocator);
                    }
                }
                return choice;
            },
            .option => |option| {
                return Node.parse(contents, option.*, rules, allocator) catch {
                    return .{ .consumed = 0, .t = .{ .terminal = .{ .content = contents[0..0] } } };
                };
            },
            .repeat => |repeat| {
                var branches = ArrayList(Node).init(allocator);
                var consumed: usize = 0;
                var start = contents;
                while (Node.parse(start, repeat.*, rules, allocator)) |node| {
                    consumed += node.consumed;
                    start = start[node.consumed..];
                    try branches.append(node);
                } else |_| {
                    if (branches.items.len == 0) {
                        return .{ .consumed = 0, .t = .{ .terminal = .{ .content = contents[0..0] } } };
                    }
                    return .{ .consumed = consumed, .t = .{ .application = .{ .branches = try branches.toOwnedSlice() } } };
                }
                unreachable;
            },
        }
    }

    fn canonicalize(self: *Node, rules: Rules, allocator: Allocator) Node {
        switch (self.t) {
            .terminal => return self.*,
            else => {},
        }
        const a = &self.t.application;
        for (a.branches) |*node| {
            node.* = node.canonicalize(rules, allocator);
        }
        const canonical = rules.rules.items[a.rule.?].canonical;
        a.rule = canonical;
        if (a.branches.len == 1) {
            const node = a.branches[0];
            switch (node.t) {
                .application => |child| {
                    if (canonical == child.rule.?) {
                        allocator.free(a.branches);
                        return node;
                    }
                },
                else => {},
            }
        }
        return self.*;
    }

    fn flatten(self: Node, rules: Rules, allocator: Allocator) ![]Node {
        switch (self.t) {
            .terminal => {
                const nodes = try allocator.alloc(Node, 1);
                nodes[0] = self;
                return nodes;
            },
            else => {},
        }
        const a = self.t.application;
        var branch_list = ArrayList(Node).init(allocator);
        for (a.branches) |node| {
            const nodes = try node.flatten(rules, allocator);
            try branch_list.appendSlice(nodes);
            allocator.free(nodes);
        }
        allocator.free(a.branches);
        var branches = branch_list.items;
        //now branches are only applications with non_temporary rules or terminals
        //only sequences and repetitions have more than one branch
        branches = if (branches.len > 1) blk: {
            var prev_was_terminal = false;
            var merge_from: usize = 0;
            var consumed: usize = 0;
            var new_len = branches.len;
            for (branches, 0..) |node, i| {
                switch (node.t) {
                    .application => {
                        if (prev_was_terminal and merge_from < i - 1) {
                            const target = &branches[merge_from];
                            const len = &target.t.terminal.content.len;
                            target.consumed += consumed - len.*;
                            len.* = consumed;
                            for (branches[merge_from + 1 .. i]) |*n| {
                                n.consumed = 0;
                            }
                            new_len -= i - merge_from - 1;
                        }
                        prev_was_terminal = false;
                    },
                    .terminal => |terminal| {
                        if (!prev_was_terminal) {
                            if (node.consumed == 0) {
                                new_len -= 1;
                                continue;
                            }
                            consumed = terminal.content.len;
                            merge_from = i;
                        } else {
                            consumed += node.consumed;
                        }
                        prev_was_terminal = true;
                    },
                }
            }
            if (prev_was_terminal and merge_from < branches.len - 1) {
                const target = &branches[merge_from];
                const len = &target.t.terminal.content.len;
                target.consumed += consumed - len.*;
                len.* = consumed;
                for (branches[merge_from + 1 .. branches.len]) |*n| {
                    n.consumed = 0;
                }
                new_len -= branches.len - merge_from - 1;
            }
            if (new_len < branches.len) {
                const branches_final = try allocator.alloc(Node, new_len);
                var i: usize = 0;
                for (branches) |n| {
                    if (n.consumed == 0) continue;
                    branches_final[i] = n;
                    i += 1;
                }
                branch_list.deinit();
                break :blk branches_final;
            } else break :blk try branch_list.toOwnedSlice();
        } else try branch_list.toOwnedSlice();
        if (a.rule != null and !rules.rules.items[a.rule.?].is_tmp) {
            var last = branches[branches.len - 1];
            switch (last.t) {
                .application => |*child| {
                    const generated_by = rules.rules.items[child.rule.?].generated_by;
                    if (generated_by == a.rule) {
                        child.rule = generated_by;
                        if (!allocator.resize(branches, branches.len - 1))
                            return error.ResizeFailed;
                        branches.len -= 1;
                        const child_branches = try allocator.alloc(Node, child.branches.len + 1);
                        @memcpy(child_branches[1..], child.branches);
                        allocator.free(child.branches);
                        child_branches[0] = self;
                        child_branches[0].t.application.branches = branches;
                        child.branches = child_branches;
                        const nodes = try allocator.alloc(Node, 1);
                        nodes[0] = last;
                        return nodes;
                    }
                },
                else => {},
            }
            const nodes = try allocator.alloc(Node, 1);
            nodes[0] = self;
            nodes[0].t.application.branches = branches;
            return nodes;
        }
        return branches;
    }

    pub fn display(self: Node, writer: anytype, rules: Rules) !void {
        switch (self.t) {
            .terminal => |terminal| {
                try writer.print("\"{s}\"", .{terminal.content});
            },
            .application => |application| {
                try writer.writeAll("{ ");
                if (application.rule) |rule| {
                    const name = rules.rules.items[rule].name;
                    try writer.print("{s} ", .{name});
                }
                for (application.branches) |node| {
                    try node.display(writer, rules);
                }
                try writer.writeAll(" }");
            },
        }
    }

    ///using DOT format
    pub fn displayGraph(self: Node, writer: anytype, rules: Rules) !void {
        try writer.writeAll("digraph {\n");
        _ = try self._displayGraph(writer, rules, 0);
        try writer.writeAll("}\n");
    }

    fn _displayGraph(self: Node, writer: anytype, rules: Rules, next_id: usize) !usize {
        var id: usize = next_id;
        switch (self.t) {
            .terminal => |terminal| try writer.print("{} [label=\"{s}\", shape=\"box\"]\n", .{ id, terminal.content }),
            .application => |application| {
                const name = rules.rules.items[application.rule.?].name;
                try writer.print("{} [label=\"{s}\"]\n", .{ next_id, name });
                id += 1;
                for (application.branches) |n| {
                    const tmp = try n._displayGraph(writer, rules, id);
                    try writer.print("{} -> {}\n", .{ next_id, id });
                    id = tmp + 1;
                }
            },
        }
        return id;
    }

    fn Iterator(comptime T: type) type {
        return struct {
            const Self = @This();
            node: *Node,
            query: T,
            idx: usize = 0,

            pub fn next(self: *Self) ?*Node {
                const branches = self.node.t.application.branches[self.idx..];
                for (branches) |*n| {
                    self.idx += 1;
                    if (n.is(self.query)) return n;
                }
                return null;
            }
        };
    }

    pub fn iter(self: *Node, query: anytype) Iterator(@TypeOf(query)) {
        return switch (self.t) {
            .application => .{ .node = self, .query = query },
            .terminal => unreachable,
        };
    }

    pub fn literal(self: Node) ![]const u8 {
        const terminal = self.literalUnchecked();
        if (terminal.len != skipToWhitespace(terminal))
            return error.InvalidLiteral;
        return terminal;
    }

    pub fn literalUnchecked(self: Node) []const u8 {
        const a = self.t.application;
        dbg.assert(a.branches.len == 1);
        return a.branches[0].t.terminal.content;
    }

    pub fn at(self: *Node, idx: usize) *Node {
        return &self.t.application.branches[idx];
    }

    pub fn is(self: Node, query: anytype) bool {
        const typeinfo = @typeInfo(@TypeOf(query));
        //dbg.print("{}\n", .{typeinfo});
        return switch (typeinfo) {
            .Pointer => |p| blk: {
                const ptrinfo = @typeInfo(p.child);
                switch (ptrinfo) {
                    .Array => |a| {
                        if (a.child != u8)
                            @compileError(INVALID_QUERY_ARG);
                        switch (self.t) {
                            .terminal => |t| break :blk mem.eql(u8, query, t.content),
                            else => break :blk false,
                        }
                    },
                    else => @compileError(INVALID_QUERY_ARG),
                }
            },
            .Int => blk: {
                const rule = @as(usize, query);
                switch (self.t) {
                    .application => |a| break :blk a.rule.? == rule,
                    else => break :blk false,
                }
            },
            else => @compileError(INVALID_QUERY_ARG),
        };
    }

    pub fn contains(self: Node, query: anytype) bool {
        for (self.t.application.branches) |n| {
            if (n.is(query)) return true;
        }
        return false;
    }

    pub fn get(self: *Node, query: anytype) ?*Node {
        return self.iter(query).next();
    }

    pub fn free(self: Node, allocator: Allocator) void {
        switch (self.t) {
            .terminal => return,
            .application => |a| {
                for (a.branches) |n| {
                    n.free(allocator);
                }
                allocator.free(a.branches);
            },
        }
    }
};
