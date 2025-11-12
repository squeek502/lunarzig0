const std = @import("std");
const builtin = @import("builtin");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
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

    var tokenizer = std.zig.Tokenizer.init(data);
    while (true) {
        const token = tokenizer.next();
        try writer.print("{},{}: {s}", .{ token.loc.start, token.loc.end, @tagName(token.tag) });
        if (builtin.os.tag == .windows) {
            try writer.writeAll("\r\n");
        } else {
            try writer.writeAll("\n");
        }
        if (token.tag == .eof) break;
    }

    try writer.flush();
}
