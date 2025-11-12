//! WARNING: Bad code ahead.
//!
//! This was originally implemented before ca6fb30e992fd86ee41a6510ef731e86f5de77c9
//! and has only been minimally hacked to get it to work again.

const std = @import("std");
const builtin = @import("builtin");

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) std.process.exit(1);

    const path = args[1];
    const data = try std.fs.cwd().readFileAllocOptions(
        path,
        allocator,
        .unlimited,
        .of(u8),
        0,
    );
    defer allocator.free(data);

    const stdout_file = std.fs.File.stdout();
    var stdout_buf: [4096]u8 = undefined;
    var stdout_writer = stdout_file.writer(&stdout_buf);
    const writer = &stdout_writer.interface;

    var ast = try std.zig.Ast.parse(allocator, data, .zig);
    defer ast.deinit(allocator);

    for (0..ast.nodes.len) |i| {
        try renderNodeData(writer, ast, @enumFromInt(i));
        try writer.writeByte('\n');
    }

    try writer.flush();
}

fn renderNodeData(writer: *std.Io.Writer, tree: std.zig.Ast, node: std.zig.Ast.Node.Index) !void {
    try writer.writeByte('<');
    try writer.print("{}:", .{@intFromEnum(node)});
    try writer.writeAll(@tagName(tree.nodeTag(node)));
    try writer.writeByte(':');
    try renderData(writer, tree, node);
    try writer.writeByte('>');
}

fn renderOptionalIndex(writer: *std.Io.Writer, node: std.zig.Ast.Node.OptionalIndex) !void {
    if (node.unwrap()) |index| {
        try writer.print("{}", .{@intFromEnum(index)});
    } else {
        try writer.writeAll("null");
    }
}

fn renderOptionalIndexes(writer: *std.Io.Writer, indexes: []const std.zig.Ast.Node.OptionalIndex) !void {
    for (indexes, 0..) |index, i| {
        try renderOptionalIndex(writer, index);
        if (i != indexes.len - 1) try writer.writeByte(',');
    }
}

const DataRepresentation = struct {
    lhs: Representation = .none,
    rhs: Representation = .none,
    list: bool = false,

    const Representation = union(enum) {
        none,
        index,
        list_zero_or_one,
        sub_range,
        fixed_list: u32,
        fn_proto_one,
        fn_proto,
        @"for",
        asm_legacy,
        @"asm",
        len_prepended_list,
    };
};

fn dataRepresentation(tag: std.zig.Ast.Node.Tag) DataRepresentation {
    return switch (tag) {
        .identifier,
        .string_literal,
        .number_literal,
        .char_literal,
        .enum_literal,
        .error_value,
        .unreachable_literal,
        .anyframe_literal,
        => .{},
        .root,
        .container_decl,
        .container_decl_trailing,
        .block,
        .block_semicolon,
        .builtin_call,
        .builtin_call_comma,
        .array_init_dot,
        .array_init_dot_comma,
        .struct_init_dot,
        .struct_init_dot_comma,
        .tagged_union,
        .tagged_union_trailing,
        => .{ .list = true },
        .container_decl_two,
        .container_decl_two_trailing,
        .block_two,
        .block_two_semicolon,
        .builtin_call_two,
        .builtin_call_two_comma,
        .array_init_dot_two,
        .array_init_dot_two_comma,
        .struct_init_dot_two,
        .struct_init_dot_two_comma,
        .tagged_union_two,
        .tagged_union_two_trailing,
        => .{ .lhs = .list_zero_or_one, .rhs = .list_zero_or_one },
        .fn_proto_simple,
        => .{ .lhs = .list_zero_or_one, .rhs = .index },
        .struct_init_one,
        .struct_init_one_comma,
        .call_one,
        .call_one_comma,
        => .{ .lhs = .index, .rhs = .list_zero_or_one },
        .switch_case_inline,
        .switch_case,
        .fn_proto_multi,
        => .{ .lhs = .sub_range, .rhs = .index },
        .switch_case_inline_one,
        .switch_case_one,
        => .{ .lhs = .list_zero_or_one, .rhs = .index },
        .local_var_decl,
        => .{ .lhs = .{ .fixed_list = 2 }, .rhs = .index },
        .ptr_type,
        => .{ .lhs = .{ .fixed_list = 3 }, .rhs = .index },
        .global_var_decl,
        => .{ .lhs = .{ .fixed_list = 4 }, .rhs = .index },
        .ptr_type_bit_range,
        => .{ .lhs = .{ .fixed_list = 5 }, .rhs = .index },
        .@"if",
        .slice,
        .while_cont,
        .array_type_sentinel,
        .container_field,
        => .{ .lhs = .index, .rhs = .{ .fixed_list = 2 } },
        .slice_sentinel,
        .@"while",
        => .{ .lhs = .index, .rhs = .{ .fixed_list = 3 } },
        .optional_type,
        .@"comptime",
        .@"try",
        .@"return",
        .@"nosuspend",
        .bool_not,
        .negation,
        .bit_not,
        .negation_wrap,
        .address_of,
        .deref,
        => .{ .lhs = .index, .rhs = .none },
        .@"switch",
        .switch_comma,
        .call,
        .call_comma,
        .array_init,
        .array_init_comma,
        .struct_init,
        .struct_init_comma,
        .container_decl_arg,
        .container_decl_arg_trailing,
        .tagged_union_enum_tag,
        .tagged_union_enum_tag_trailing,
        => .{ .lhs = .index, .rhs = .sub_range },
        .fn_proto_one,
        => .{ .lhs = .fn_proto_one, .rhs = .index },
        .fn_proto,
        => .{ .lhs = .fn_proto, .rhs = .index },
        .@"asm",
        => .{ .lhs = .index, .rhs = .@"asm" },
        .asm_legacy,
        => .{ .lhs = .index, .rhs = .asm_legacy },
        .@"defer",
        => .{ .lhs = .index, .rhs = .none },
        .@"for",
        => .{ .lhs = .@"for", .rhs = .none },
        .assign_destructure,
        => .{ .lhs = .len_prepended_list, .rhs = .index },
        else => .{ .lhs = .index, .rhs = .index },
    };
}

fn renderData(writer: *std.Io.Writer, tree: std.zig.Ast, node: std.zig.Ast.Node.Index) !void {
    // Instead of dealing with each node type individually and accessing the correct union tag,
    // we just group nodes together and access arbitrary union tags to pull out the data
    // we want.
    //
    // This is a VERY hacky way to allow interpreting node data as whatever union tag we want
    // instead of having to know the correct one.
    @setRuntimeSafety(false);

    const repr = dataRepresentation(tree.nodeTag(node));

    try writer.print("{}", .{tree.nodeMainToken(node)});

    if (!repr.list and repr.lhs == .none and repr.rhs == .none) {
        return;
    }

    if (repr.list or repr.lhs != .none) {
        try writer.writeByte(':');
    }

    if (repr.list) {
        const members = tree.extraDataSlice(tree.nodeData(node).extra_range, std.zig.Ast.Node.Index);
        for (members, 0..) |member_i, i| {
            try writer.print("{}", .{@intFromEnum(member_i)});
            if (i != members.len - 1) try writer.writeByte(',');
        }
        return;
    }

    switch (repr.lhs) {
        .none => {},
        .index => {
            try renderOptionalIndex(writer, tree.nodeData(node).opt_node);
        },
        .list_zero_or_one => {
            if (tree.nodeData(node).opt_node.unwrap()) |index| {
                try writer.print("{}", .{@intFromEnum(index)});
            }
        },
        .sub_range => {
            const extra_index, _ = tree.nodeData(node).extra_and_node;
            const extra_range = tree.extraData(extra_index, std.zig.Ast.Node.SubRange);
            const nodes = tree.extraDataSlice(extra_range, std.zig.Ast.Node.Index);
            for (nodes, 0..) |node_i, i| {
                try writer.print("{}", .{@intFromEnum(node_i)});
                if (i != nodes.len - 1) try writer.writeByte(',');
            }
        },
        .fixed_list => |len| {
            const extra_index, _ = tree.nodeData(node).extra_and_node;
            const members = tree.extraDataSliceWithLen(extra_index, len, std.zig.Ast.Node.OptionalIndex);
            try renderOptionalIndexes(writer, members);
        },
        .fn_proto_one => {
            const extra_index, _ = tree.nodeData(node).extra_and_node;
            const members = tree.extraDataSliceWithLen(extra_index, 5, std.zig.Ast.Node.OptionalIndex);
            if (members[0].unwrap()) |param_index| {
                try writer.print("{},", .{@intFromEnum(param_index)});
            }
            try renderOptionalIndexes(writer, members[1..]);
        },
        .fn_proto => {
            const extra_index, _ = tree.nodeData(node).extra_and_node;
            const start_i = tree.extra_data[@intFromEnum(extra_index)];
            const end_i = tree.extra_data[@intFromEnum(extra_index) + 1];
            const nodes = tree.extra_data[start_i..end_i];
            for (nodes) |node_i| {
                try writer.print("{}", .{node_i});
                try writer.writeByte(',');
            }
            const members = tree.extra_data[@intFromEnum(extra_index) + 2 ..][0..4];
            try renderOptionalIndexes(writer, @ptrCast(members));
        },
        .@"for" => {
            const full = tree.forFull(node);
            for (full.ast.inputs) |input_i| {
                try writer.print("{}", .{@intFromEnum(input_i)});
                try writer.writeByte(',');
            }
            try writer.print("{},", .{@intFromEnum(full.ast.then_expr)});
            try renderOptionalIndex(writer, full.ast.else_expr);
        },
        .@"asm", .asm_legacy => unreachable,
        .len_prepended_list => {
            const extra_index, _ = tree.nodeData(node).extra_and_node;
            const len = tree.extra_data[@intFromEnum(extra_index)];
            const nodes = tree.extra_data[@intFromEnum(extra_index) + 1 ..][0..len];
            for (nodes, 0..) |node_i, i| {
                try writer.print("{}", .{node_i});
                if (i != nodes.len - 1) try writer.writeByte(',');
            }
        },
    }

    switch (repr.rhs) {
        .none => return,
        .list_zero_or_one => if (repr.lhs != .list_zero_or_one) {
            try writer.writeByte(':');
        },
        else => {
            try writer.writeByte(':');
        },
    }

    switch (repr.rhs) {
        .none => {},
        .index => {
            const lhs, const rhs = tree.nodeData(node).node_and_opt_node;
            _ = lhs;
            try renderOptionalIndex(writer, rhs);
        },
        .list_zero_or_one => {
            const lhs, const rhs = tree.nodeData(node).node_and_opt_node;
            _ = lhs;
            if (rhs.unwrap()) |index| {
                if (repr.lhs == .list_zero_or_one) {
                    try writer.writeByte(',');
                }
                try writer.print("{}", .{@intFromEnum(index)});
            }
        },
        .sub_range => {
            const lhs, const extra_index = tree.nodeData(node).node_and_extra;
            _ = lhs;
            const extra_range = tree.extraData(extra_index, std.zig.Ast.Node.SubRange);
            const nodes = tree.extraDataSlice(extra_range, std.zig.Ast.Node.Index);
            for (nodes, 0..) |node_i, i| {
                try writer.print("{}", .{@intFromEnum(node_i)});
                if (i != nodes.len - 1) try writer.writeByte(',');
            }
        },
        .fixed_list => |len| {
            const lhs, const extra_index = tree.nodeData(node).node_and_extra;
            _ = lhs;
            const members = tree.extraDataSliceWithLen(extra_index, len, std.zig.Ast.Node.OptionalIndex);
            try renderOptionalIndexes(writer, members);
        },
        .fn_proto, .fn_proto_one => unreachable,
        .@"for" => {},
        .asm_legacy => {
            const lhs, const extra_index = tree.nodeData(node).node_and_extra;
            _ = lhs;
            const asm_legacy = tree.extraData(extra_index, std.zig.Ast.Node.AsmLegacy);
            const extra_range = std.zig.Ast.Node.SubRange{ .start = asm_legacy.items_start, .end = asm_legacy.items_end };
            const nodes = tree.extraDataSlice(extra_range, std.zig.Ast.Node.Index);
            for (nodes, 0..) |node_i, i| {
                try writer.print("{}", .{@intFromEnum(node_i)});
                if (i != nodes.len - 1) try writer.writeByte(',');
            }
            try writer.writeByte(':');
            try writer.print("{}", .{asm_legacy.rparen});
        },
        .@"asm" => {
            const lhs, const extra_index = tree.nodeData(node).node_and_extra;
            _ = lhs;
            const asm_legacy = tree.extraData(extra_index, std.zig.Ast.Node.Asm);
            const extra_range = std.zig.Ast.Node.SubRange{ .start = asm_legacy.items_start, .end = asm_legacy.items_end };
            const nodes = tree.extraDataSlice(extra_range, std.zig.Ast.Node.Index);
            for (nodes, 0..) |node_i, i| {
                try writer.print("{}", .{@intFromEnum(node_i)});
                if (i != nodes.len - 1) try writer.writeByte(',');
            }
            if (nodes.len > 0) {
                try writer.writeByte(',');
            }
            try renderOptionalIndex(writer, asm_legacy.clobbers);
            try writer.writeByte(',');
            try writer.print("{}", .{asm_legacy.rparen});
        },
        .len_prepended_list => {
            const lhs, const extra_index = tree.nodeData(node).node_and_extra;
            _ = lhs;
            const len = tree.extra_data[@intFromEnum(extra_index)];
            const nodes = tree.extra_data[@intFromEnum(extra_index) + 1 ..][0..len];
            for (nodes, 0..) |node_i, i| {
                try writer.print("{}", .{node_i});
                if (i != nodes.len - 1) try writer.writeByte(',');
            }
        },
    }
}

fn renderToken(writer: *std.Io.Writer, tree: std.zig.Ast, token: std.zig.Ast.TokenIndex) !void {
    try writer.writeAll(tree.tokenSlice(token));
}
