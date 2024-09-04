const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) std.process.exit(1);

    const dirpath = args[1];
    var dir = try std.fs.cwd().openDir(dirpath, .{ .iterate = true });
    defer dir.close();

    var pathbuf = std.ArrayList(u8).init(allocator);
    defer pathbuf.deinit();

    try pathbuf.appendSlice(dirpath);
    if (!std.fs.path.isSep(dirpath[dirpath.len - 1])) {
        try pathbuf.append(std.fs.path.sep);
    }
    const pathbuf_len = pathbuf.items.len;

    var it = try dir.walk(allocator);
    defer it.deinit();
    while (try it.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.eql(u8, std.fs.path.extension(entry.basename), ".zig")) continue;
        std.debug.print("{s}\n", .{entry.path});

        pathbuf.shrinkRetainingCapacity(pathbuf_len);
        try pathbuf.appendSlice(entry.path);

        const lua_tokens = try getLua(allocator, pathbuf.items);
        defer allocator.free(lua_tokens);

        const zig_tokens = try getZig(allocator, pathbuf.items);
        defer allocator.free(zig_tokens);

        std.testing.expectEqualStrings(zig_tokens, lua_tokens) catch |err| {
            std.debug.print("{s}\n", .{pathbuf.items});
            return err;
        };
    }
}

fn getLua(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{
            "lua",
            "dump_tokens.lua",
            path,
        },
        .max_output_bytes = std.math.maxInt(usize),
    });
    defer allocator.free(result.stderr);

    if (result.term != .Exited or result.term.Exited != 0) return error.ProcessFailed;
    return result.stdout;
}

fn getZig(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{
            "dump_tokens",
            path,
        },
        .max_output_bytes = std.math.maxInt(usize),
    });
    defer allocator.free(result.stderr);

    if (result.term != .Exited or result.term.Exited != 0) return error.ProcessFailed;
    return result.stdout;
}
