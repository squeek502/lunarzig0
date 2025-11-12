const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) std.process.exit(1);

    const dirpath = args[1];
    var dir = std.fs.cwd().openDir(dirpath, .{ .iterate = true }) catch |err| switch (err) {
        error.NotDir => {
            try check(allocator, dirpath);
            return;
        },
        else => |e| return e,
    };
    defer dir.close();

    var pathbuf: std.ArrayList(u8) = .empty;
    defer pathbuf.deinit(allocator);

    try pathbuf.appendSlice(allocator, dirpath);
    if (!std.fs.path.isSep(dirpath[dirpath.len - 1])) {
        try pathbuf.append(allocator, std.fs.path.sep);
    }
    const pathbuf_len = pathbuf.items.len;

    var it = try dir.walk(allocator);
    defer it.deinit();
    while (try it.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.eql(u8, std.fs.path.extension(entry.basename), ".zig")) continue;
        std.debug.print("{s}\n", .{entry.path});

        pathbuf.shrinkRetainingCapacity(pathbuf_len);
        try pathbuf.appendSlice(allocator, entry.path);

        try check(allocator, pathbuf.items);
    }
}

fn check(allocator: std.mem.Allocator, path: []const u8) !void {
    const lua = try getLua(allocator, path);
    defer allocator.free(lua);

    const zig = try getZig(allocator, path);
    defer allocator.free(zig);

    std.testing.expectEqualStrings(zig, lua) catch |err| {
        std.debug.print("{s}\n", .{path});
        return err;
    };

    var line_it = std.mem.tokenizeScalar(u8, lua, '\n');
    while (line_it.next()) |line| {
        if (std.mem.indexOf(u8, line, "TODO") != null) {
            std.debug.print("{s}\n", .{line});
        }
    }
}

fn getLua(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{
            "lua",
            "dump_ast.lua",
            path,
        },
        .max_output_bytes = std.math.maxInt(usize),
    });
    defer allocator.free(result.stderr);
    errdefer allocator.free(result.stdout);

    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("{s}\n", .{result.stdout});
        std.debug.print("{s}\n", .{result.stderr});
        return error.ProcessFailed;
    }
    return result.stdout;
}

fn getZig(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{
            "./dump_ast",
            path,
        },
        .max_output_bytes = std.math.maxInt(usize),
    });
    defer allocator.free(result.stderr);
    errdefer allocator.free(result.stdout);

    if (result.term != .Exited or result.term.Exited != 0) {
        std.debug.print("{s}\n", .{result.stdout});
        std.debug.print("{s}\n", .{result.stderr});
        return error.ProcessFailed;
    }
    return result.stdout;
}
