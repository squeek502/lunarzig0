local parser = require('parser')
local render = require('render')

local function read(path)
  local file = assert(io.open(path, "rb"))
  local data = file:read("*all")
  file:close()
  return data
end

local path = arg[1]
local source = read(path)
source = source .. '\000'

local ast = assert(parser.parse(source))
local rendered = render.renderList(ast)
io.write(rendered)
