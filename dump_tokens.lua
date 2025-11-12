local Tokenizer = require('tokenizer').Tokenizer

local function read(path)
  local file = assert(io.open(path, "rb"))
  local data = file:read("*all")
  file:close()
  return data
end

local function dump(token)
  -- 1-indexed to 0-indexed
  local zig_start = token.start - 1
  -- zig's end index is exclusive
  local zig_end = token["end"]
  -- zig eof token is zero length
  if token.tag == "eof" then
    zig_end = zig_end - 1
  end
  print(string.format("%d,%d: %s", zig_start, zig_end, token.tag))
end

local path = arg[1]

local source = read(path) .. '\000'

local tokenizer = Tokenizer.init(source)
while true do
  local tok = tokenizer:next()
  dump(tok)
  if tok.tag == "eof" then break end
end
