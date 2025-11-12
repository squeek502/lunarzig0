local enum = require('enum')
local lexemes = require('tokenizer').lexemes

local Container = enum.fromTable{
  enum = 1,
  tuple = 2,
  other = 3,
}

local Space = enum.fromTable{
  newline = 1,
}

local Render = {}
Render.__index = Render

function Render.init(tree)
  local self = setmetatable({}, Render)
  self.tree = tree
  self.buffer = {}
  self.match_zig = true
  return self
end

function Render:renderIndex(i)
  assert(i ~= nil, "this is a bug")
  if self.match_zig then
    if i then
      table.insert(self.buffer, tostring(i - 1))
    else
      table.insert(self.buffer, 'null')
    end
  else
    table.insert(self.buffer, tostring(i))
  end
end

function Render:renderNodeData(node_i)
  local node = self.tree.nodes[node_i]
  local tag_name = node.tag
  if self.match_zig then
    if tag_name == "container_decl" or tag_name == "container_decl_trailing" then
      if #node.lhs <= 2 then
        tag_name = tag_name == "container_decl" and "container_decl_two" or "container_decl_two_trailing"
      end
    elseif tag_name == "block" or tag_name == "block_semicolon" then
      if #node.lhs <= 2 then
        tag_name = tag_name == "block" and "block_two" or "block_two_semicolon"
      end
    elseif tag_name == "builtin_call" or tag_name == "builtin_call_comma" then
      if #node.lhs <= 2 then
        tag_name = tag_name == "builtin_call" and "builtin_call_two" or "builtin_call_two_comma"
      end
    elseif tag_name == "call" or tag_name == "call_comma" then
      if #node.rhs <= 1 then
        tag_name = tag_name == "call" and "call_one" or "call_one_comma"
      end
    elseif tag_name == "switch_case" or tag_name == "switch_case_inline" then
      if #node.lhs <= 1 then
        tag_name = tag_name == "switch_case" and "switch_case_one" or "switch_case_inline_one"
      end
    elseif tag_name == "struct_init" or tag_name == "struct_init_comma" then
      if #node.rhs <= 1 then
        tag_name = tag_name == "struct_init" and "struct_init_one" or "struct_init_one_comma"
      end
    elseif tag_name == "struct_init_dot" or tag_name == "struct_init_dot_comma" then
      if #node.lhs <= 2 then
        tag_name = tag_name == "struct_init_dot" and "struct_init_dot_two" or "struct_init_dot_two_comma"
      end
    elseif tag_name == "array_init_dot" or tag_name == "array_init_dot_comma" then
      if #node.lhs <= 2 then
        tag_name = tag_name == "array_init_dot" and "array_init_dot_two" or "array_init_dot_two_comma"
      end
    elseif tag_name == "fn_proto_simple" then
      if #node.lhs > 1 then
        tag_name = "fn_proto_multi"
      end
    elseif tag_name == "fn_proto" then
      if #node.lhs[1] <= 1 then
        tag_name = "fn_proto_one"
      end
    elseif tag_name == "array_init" or tag_name == "array_init_comma" then
      if #node.rhs <= 1 then
        tag_name = tag_name == "array_init" and "array_init_one" or "array_init_one_comma"
      end
    elseif tag_name == "tagged_union" or tag_name == "tagged_union_trailing" then
      if #node.lhs <= 2 then
        tag_name = tag_name == "tagged_union" and "tagged_union_two" or "tagged_union_two_trailing"
      end
    end
  end
  table.insert(self.buffer, '<')
  self:renderIndex(node_i)
  table.insert(self.buffer, ':'..tag_name..':')
  self:renderData(node_i)
  table.insert(self.buffer, '>')
end

function Render:renderIndexList(list)
  for i, index in ipairs(list) do
    -- somewhat of a special case for fn_proto
    if type(index) == "table" then
      self:renderIndexList(index)
      if #index > 0 and i ~= #list then
        table.insert(self.buffer, ',')
      end
    else
      self:renderIndex(index)
      if i ~= #list then table.insert(self.buffer, ',') end
    end
  end
end

function Render:renderData(node_i)
  local node = self.tree.nodes[node_i]
  self:renderIndex(node.main_token)

  if node.lhs ~= nil then
    table.insert(self.buffer, ':')

    if type(node.lhs) == "table" then
      self:renderIndexList(node.lhs)
    else
      self:renderIndex(node.lhs)
    end
  end

  if node.rhs ~= nil then
    table.insert(self.buffer, ':')

    if type(node.rhs) == "table" then
      self:renderIndexList(node.rhs)
    else
      self:renderIndex(node.rhs)
    end
  end
end

function Render:renderToken(token_i)
  table.insert(self.buffer, self:getTokenLexeme(token_i))
end

function Render:getTokenLexeme(token_i)
  return self.tree.source:sub(self.tree.tokens.starts[token_i], self.tree.tokens.ends[token_i])
end

local function renderList(tree)
  local r = Render.init(tree)
  for node_i in ipairs(tree.nodes) do
    local ok, err = pcall(function()
      r:renderNodeData(node_i)
    end)
    if not ok then
      print(node_i, tree.nodes[node_i].tag)
    end
    assert(ok, err)
    table.insert(r.buffer, '\n')
  end
  return table.concat(r.buffer)
end

return {
  Render = Render,
  renderTree = renderTree,
  renderList = renderList,
}