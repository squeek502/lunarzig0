local getTokens = require('Tokenizer').getTokens

local path = arg[1]

local function read(path)
  local file = assert(io.open(path, "rb"))
  local data = file:read("*all")
  file:close()
  return data
end

local source = read(path)
local tokens = getTokens(source)
for _, token in ipairs(tokens) do
  -- 1-indexed to 0-indexed
  local zig_start = token.loc.start - 1
  -- zig's end index is exclusive
  local zig_end = token.loc["end"]
  -- zig eof token is zero length
  if token.tag == "eof" then
    zig_end = zig_end - 1
  end
  print(string.format("%d,%d: %s", zig_start, zig_end, token.tag))
end
