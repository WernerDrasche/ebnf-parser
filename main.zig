const std = @import("std");
const ebnf = @import("ebnf/ebnf.zig");
const T = @import("nodetypes.zig").T;
const Node = ebnf.Node;
const Rules = ebnf.Rules;
const dbg = std.debug;
const StringHashMap = std.StringHashMap;

const MAX_FILE_SIZE = 1 << 20;
var GPA = std.heap.GeneralPurposeAllocator(.{}){};
var allocator = GPA.allocator();
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const stdin = std.io.getStdIn().reader();

const Context = struct {
    const Variables = StringHashMap(i64);
    gvars: Variables,

    fn getVariable(self: Context, name: *Node) !Variables.Entry {
        const ident = name.literal() catch return error.SyntaxError;
        if (self.gvars.getEntry(ident)) |entry| return entry;
        try stderr.print("ERROR: variable {s} used before declaration\n", .{ident});
        return error.RuntimeError;
    }
};

fn interpret(root: *Node) !void {
    var gvars = StringHashMap(i64).init(allocator);
    var decls = root.iter(@intFromEnum(T.decl));
    while (decls.next()) |decl| {
        var names = decl.iter(@intFromEnum(T.name));
        while (names.next()) |name| {
            try gvars.put(try name.literal(), 0);
        }
    }
    var ctx = Context{ .gvars = gvars };
    defer ctx.gvars.deinit();
    var stmts = root.iter(@intFromEnum(T.stmt));
    while (stmts.next()) |stmt| {
        try evalStmt(stmt, &ctx);
    }
}

fn evalStmt(stmt: *Node, ctx: *Context) !void {
    var first = stmt.at(0);
    if (first.is("{")) {
        var stmts = stmt.iter(@intFromEnum(T.stmt));
        while (stmts.next()) |s| {
            try evalStmt(s, ctx);
        }
    } else if (stmt.contains("=")) {
        const lhs = first;
        var rhs = stmt.at(2);
        const number = if (rhs.is(@intFromEnum(T.read))) blk: {
            const read = try stdin.readUntilDelimiterAlloc(allocator, '\n', 128);
            defer allocator.free(read);
            break :blk std.fmt.parseInt(i64, read, 0) catch {
                try stderr.print("ERROR: invalid number: {s}\n", .{read});
                return error.RuntimeError;
            };
        } else try evalExpr(rhs, ctx);
        const variable = try ctx.getVariable(lhs);
        variable.value_ptr.* = number;
    } else if (first.is(@intFromEnum(T.write))) {
        const number = try evalExpr(stmt.at(2), ctx);
        try stdout.print("{}\n", .{number});
    } else if (first.is(@intFromEnum(T.@"if"))) {
        const boolean = try evalCond(stmt.at(2), ctx);
        var stmts = stmt.iter(@intFromEnum(T.stmt));
        var true_branch = stmts.next().?;
        if (boolean)
            try evalStmt(true_branch, ctx)
        else if (stmts.next()) |else_branch|
            try evalStmt(else_branch, ctx);
    } else if (first.is(@intFromEnum(T.@"while"))) {
        while (try evalCond(stmt.at(2), ctx)) {
            try evalStmt(stmt.at(4), ctx);
        }
    }
}

fn evalCond(cond: *Node, ctx: *Context) !bool {
    var first = cond.at(0);
    if (first.is("false")) return false;
    if (first.is("true")) return true;
    if (first.is(@intFromEnum(T.expr))) {
        var left = try evalExpr(first, ctx);
        var right = try evalExpr(cond.at(2), ctx);
        const comp = cond.at(1).literalUnchecked();
        if (comp.len == 1) {
            return if (comp[0] == '<') left < right else left > right;
        }
        return switch (comp[0]) {
            '!' => left != right,
            '=' => left == right,
            '<' => left <= right,
            '>' => left >= right,
            else => unreachable,
        };
    }
    if (first.is(@intFromEnum(T.bunop))) {
        const boolean = try evalCond(cond.at(1), ctx);
        return !boolean;
    }
    var middle = cond.at(1);
    if (middle.is(@intFromEnum(T.bbinop))) {
        var left = try evalCond(first, ctx);
        var right = try evalCond(cond.at(2), ctx);
        const bbinop = middle.literalUnchecked();
        return if (bbinop[0] == '&') left and right else left or right;
    }
    return try evalCond(middle, ctx);
}

fn evalExpr(expr: *Node, ctx: *Context) !i64 {
    const first = expr.at(0);
    if (first.is(@intFromEnum(T.number))) {
        const literal = first.literal() catch return error.SyntaxError;
        const number = std.fmt.parseInt(i64, literal, 0) catch {
            try stderr.print("ERROR: invalid number: {s}\n", .{literal});
            return error.SyntaxError;
        };
        return number;
    }
    if (first.is(@intFromEnum(T.name))) {
        const variable = try ctx.getVariable(first);
        return variable.value_ptr.*;
    }
    if (first.is(@intFromEnum(T.unop))) {
        const value = try evalExpr(expr.at(1), ctx);
        return -value;
    }
    var middle = expr.at(1);
    if (middle.is(@intFromEnum(T.binop))) {
        const left = try evalExpr(first, ctx);
        const right = try evalExpr(expr.at(2), ctx);
        const binop = expr.at(1).literalUnchecked();
        return switch (binop[0]) {
            '+' => left + right,
            '-' => left - right,
            '*' => left * right,
            '/' => @divTrunc(left, right),
            '%' => @rem(left, right),
            else => unreachable,
        };
    }
    return try evalExpr(middle, ctx);
}

fn print_usage(exe_name: []const u8) noreturn {
    stdout.print("Usage: {s} program_path [ast_graph_output_path]\n", .{exe_name}) catch unreachable;
    std.process.exit(1);
}

pub fn main() !void {
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    const exe_name = args.next().?;
    const program_path = args.next() orelse print_usage(exe_name);
    const graph_path = args.next() orelse "";

    const cwd = std.fs.cwd();
    const grammar = @embedFile("grammar.ebnf");
    var rules = try Rules.create(grammar, allocator);
    defer rules.free();
    //try rules.display(stdout);

    const enum_dump = try cwd.createFile("nodetypes.zig", .{});
    defer enum_dump.close();
    try rules.displayEnum(enum_dump.writer(), "T");

    const program = try cwd.readFileAlloc(allocator, program_path, MAX_FILE_SIZE);
    defer allocator.free(program);
    var timer = try std.time.Timer.start();
    var ast = try rules.parse(program, @intFromEnum(T.program));
    defer ast.free(allocator);
    const elapsed = timer.read();
    const ms = elapsed / 1000_000;
    const frac = (elapsed / 10_000) % 100;
    try stdout.print("parsing took {}.{}ms\n", .{ ms, frac });

    dbg.print("{s}\n", .{program[0..ast.consumed]});
    if (program.len != ast.consumed + ebnf.skipWhitespace(program[ast.consumed..]))
        return error.InvalidProgram;

    if (graph_path.len != 0) {
        const graph = try cwd.createFile(graph_path, .{});
        defer graph.close();
        try ast.displayGraph(graph.writer(), rules);
        try stdout.print("output ast graph saved in {s}\n", .{graph_path});
    } else {
        try stdout.print("starting interpreter\n", .{});
        try interpret(&ast);
    }
}

test "memleak" {
    allocator = std.testing.allocator;
    dbg.print("\n", .{});
    try main();
}
