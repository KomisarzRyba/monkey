const std = @import("std");
const object = @import("object/object.zig");

allocator: *std.heap.ArenaAllocator,
store: std.StringHashMap(object.Object),
outer: ?*Self = null,

const Self = @This();

pub fn init(alloc: *std.heap.ArenaAllocator) Self {
    return Self{
        .allocator = alloc,
        .store = .init(alloc.allocator()),
    };
}

pub fn init_enclosed(alloc: *std.heap.ArenaAllocator, outer: *Self) Self {
    return Self{
        .allocator = alloc,
        .store = .init(alloc.allocator()),
        .outer = outer,
    };
}

pub fn deinit(self: *Self) void {
    self.store.deinit();
}

pub fn get(self: *Self, name: []const u8) ?object.Object {
    const value = self.store.get(name);
    if (value) |v| return v;
    if (self.outer) |outer| return outer.get(name);
    return null;
}

pub fn set(self: *Self, name: []const u8, value: object.Object) !void {
    try self.store.put(name, value);
}
