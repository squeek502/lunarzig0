local Ast = {}
Ast.__index = Ast

function Ast.init(source, tokens, nodes)
	local self = setmetatable({}, Ast)
	self.source = source
	self.tokens = tokens
	self.nodes = nodes
	return self
end

function Ast:rootDecls()
	return self.nodes[1].lhs
end

function Ast:tokenSlice(token_i)
  return self.source:sub(
    self.tokens.starts[token_i],
    self.tokens.ends[token_i]
  )
end

function Ast:fullFnProto(node_i)
  local node = self.nodes[node_i]
  local info
  if node.tag == "fn_proto_simple" then
    info = {
      proto_node = node_i,
      fn_token = node.main_token,
      return_type = node.rhs,
      params = node.lhs,
    }
  elseif node.tag == "fn_proto" then
    info = {
      proto_node = node_i,
      fn_token = node.main_token,
      return_type = node.rhs,
      params = node.lhs[1],
      align_expr = node.lhs[2],
      addrspace_expr = node.lhs[3],
      section_expr = node.lhs[4],
      callconv_expr = node.lhs[5],
    }
  elseif node.tag == "fn_decl" then
    return self:fullFnProto(node.lhs)
  end

  return self:fullFnProtoComponents(info)
end

function Ast:fullFnProtoComponents(info)
  local result = {
    ast = info,
    visib_token = nil,
    extern_export_inline_token = nil,
    lib_name = nil,
    name_token = nil,
    lparen = nil,
  }
  local i = info.fn_token
  while i > 0 do
    i = i - 1
    local tok_tag = self.tokens.tags[i]
    if
      tok_tag == "keyword_extern" or
      tok_tag == "keyword_export" or
      tok_tag == "keyword_inline" or
      tok_tag == "keyword_noinline"
    then
      result.extern_export_inline_token = i
    elseif tok_tag == "keyword_pub" then
      result.visib_token = i
    elseif tok_tag == "string_literal" then
      result.lib_name = i
    else
      break
    end
  end
  local after_fn_token = info.fn_token + 1
  if self.tokens.tags[after_fn_token] == "identifier" then
    result.name_token = after_fn_token
    result.lparen = after_fn_token + 1
  else
    result.lparen = after_fn_token
  end
  assert(self.tokens.tags[result.lparen] == "l_paren")

  return result
end

return Ast