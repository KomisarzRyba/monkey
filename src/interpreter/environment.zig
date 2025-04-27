const std = @import("std");
const object = @import("object/object.zig");

store: std.StringHashMap(object.Object),

const Self = @This();

pub fn init(allocator: std.mem.Allocator) Self {
    return Self{
        .store = .init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.store.deinit();
}

pub fn get(self: *Self, name: []const u8) ?object.Object {
    return self.store.get(name);
}

pub fn set(self: *Self, name: []const u8, value: object.Object) !void {
    try self.store.put(name, value);
}
