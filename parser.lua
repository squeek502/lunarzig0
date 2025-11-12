local Tokenizer = require('tokenizer').Tokenizer
local token_lexemes = require('tokenizer').lexemes
local isWhitespace = require('tokenizer').isWhitespace
local Ast = require('ast')
local enum = require('enum')
local unpack = table.unpack or unpack

local undefined = nil

local Parser = {}
Parser.__index = Parser

function Parser.init(tokens, source)
  local self = setmetatable({}, Parser)
  self.tokens = tokens
  self.source = source
  self.nodes = {}
  self.scratch = {}
  self.tok_i = 1
  return self
end

function Parser:tokenTag()
  return self.tokens.tags[self.tok_i]
end

function Parser:assertToken(tag)
  local token_i = self:nextToken()
  assert(self.tokens.tags[token_i] == tag)
  return token_i
end

-- Returns the current token, but advances to the next
function Parser:nextToken()
  local tok_i = self.tok_i
  self.tok_i = self.tok_i + 1
  return tok_i
end

function Parser:parseRoot()
  local root_i = self:addNode({
    tag = "root",
    main_token = 1, -- first token, whatever it is
    lhs = undefined,
    rhs = undefined,
  })
  local root_members = self:parseContainerMembers()
  if self.tokens.tags[self.tok_i] ~= "eof" then
    error("expected eof")
  end
  assert(#self.scratch == 0, "unbalanced scratch, remaining: "..(#self.scratch))
  self.nodes[root_i].lhs = root_members.list
end

local FieldState = enum.fromArray{
  "none",
  "seen",
  "end",
}

function Parser:parseContainerMembers()
  local scratch_top = #self.scratch

  local field_state = FieldState.none

  while self:eatToken("container_doc_comment") do end

  local trailing = false
  while true do
    local doc_comment = self:eatDocComments()

    local token_tag = self:tokenTag()
    if token_tag == "keyword_test" then
      if doc_comment then
        error("test_doc_comment")
      end
      local test_decl_node = self:expectTestDecl()
      if test_decl_node then
        if field_state == FieldState.seen then
          field_state = FieldState["end"]
        end
        table.insert(self.scratch, test_decl_node)
      end
      trailing = false
    elseif token_tag == "keyword_comptime" then
      local next_token_tag = self.tokens.tags[self.tok_i + 1]
      if next_token_tag == "l_brace" then
        assert(not doc_comment, "comptime_doc_comment")
        local comptime_token = self:nextToken()
        local block = assert(self:parseBlock())
        local comptime_node = self:addNode({
          tag = "comptime",
          main_token = comptime_token,
          lhs = block,
          rhs = undefined,
        })
        if field_state == FieldState.seen then
          field_state = FieldState["end"]
        end
        table.insert(self.scratch, comptime_node)
        trailing = false
      else
        local container_field = self:expectContainerField()
        if field_state == FieldState.none then
          field_state = FieldState.seen
        elseif field_state == FieldState["end"] then
          error("decl_between_fields")
        end
        table.insert(self.scratch, container_field)
        local tok_tag = self:tokenTag()
        local continue = false
        if tok_tag == "comma" then
          self:nextToken()
          trailing = true
          continue = true
        elseif
          tok_tag == "r_brace" or
          tok_tag == "eof"
        then
          trailing = false
          break
        end

        if not continue then
          error("expected_comma_after_field")
        end
      end
    elseif token_tag == "keyword_pub" then
      self:nextToken()
      local top_level_decl = self:expectTopLevelDecl()
      if top_level_decl then
        if field_state == FieldState.seen then
          field_state = FieldState["end"]
        end
        table.insert(self.scratch, top_level_decl)
      end
      trailing = self.tokens.tags[self.tok_i - 1] == "semicolon"
    elseif
      token_tag == "keyword_const" or
      token_tag == "keyword_var" or
      token_tag == "keyword_threadlocal" or
      token_tag == "keyword_export" or
      token_tag == "keyword_extern" or
      token_tag == "keyword_inline" or
      token_tag == "keyword_noinline" or
      token_tag == "keyword_fn"
    then
      local top_level_decl = self:expectTopLevelDecl()
      if top_level_decl then
        if field_state == FieldState.seen then
          field_state = FieldState["end"]
        end
        table.insert(self.scratch, top_level_decl)
      end
      trailing = self.tokens.tags[self.tok_i - 1] == "semicolon"
    elseif
      token_tag == "eof" or
      token_tag == "r_brace"
    then
      if doc_comment then
        error("unattached_doc_comment")
      end
      break
    else
      -- TODO? parseCStyleContainer, I think it's only there to provide nicer error messages
      local container_field = self:expectContainerField()

      if field_state == FieldState.none then
        field_state = FieldState.seen
      elseif field_state == FieldState["end"] then
        error("decl_between_fields")
      end

      table.insert(self.scratch, container_field)
      local tok_tag = self.tokens.tags[self.tok_i]
      local continue = false
      if tok_tag == "comma" then
        self:nextToken()
        trailing = true
        continue = true
      elseif tok_tag == "r_brace" or tok_tag == "eof" then
        trailing = false
        break
      end

      if not continue then
        error("expected_comma_after_field")
      end
    end
  end

  return {
    list = self:sliceAndRestoreScratch(scratch_top),
    trailing = trailing,
  }
end

function Parser:expectTestDecl()
  local test_token = self:assertToken("keyword_test")
  local name_token = false
  if self:tokenTag() == "string_literal" or self:tokenTag() == "identifier" then
    name_token = self:nextToken()
  end
  local block_node = self:parseBlock()
  if not block_node then
    error("expected_block")
  end
  return self:addNode({
    tag = "test_decl",
    main_token = test_token,
    lhs = name_token,
    rhs = block_node,
  })
end

function Parser:sliceAndRestoreScratch(scratch_top)
  local slice = {unpack(self.scratch, scratch_top + 1)}
  self:restoreScratch(scratch_top)
  return slice
end

-- restore scratch to initial length
function Parser:restoreScratch(scratch_top)
  while #self.scratch > scratch_top do
    self.scratch[#self.scratch] = nil
  end
end

function Parser:expectContainerField()
  self:eatToken("keyword_comptime")
  local main_token = self.tok_i
  if self:tokenTag() == "identifier" and self.tokens.tags[self.tok_i + 1] == "colon" then
    self.tok_i = self.tok_i + 2
  end
  local type_expr = self:expectTypeExpr()
  local align_expr = self:parseByteAlign()
  local value_expr = false
  if self:eatToken("equal") then
    value_expr = self:expectExpr()
  end

  if not align_expr then
    return self:addNode({
      tag = "container_field_init",
      main_token = main_token,
      lhs = type_expr,
      rhs = value_expr,
    })
  elseif not value_expr then
    return self:addNode({
      tag = "container_field_align",
      main_token = main_token,
      lhs = type_expr,
      rhs = align_expr,
    })
  else
    return self:addNode({
      tag = "container_field",
      main_token = main_token,
      lhs = type_expr,
      rhs = { align_expr, value_expr },
    })
  end
end

function Parser:expectTopLevelDecl()
  local extern_export_inline_token_i = self:nextToken()
  local is_extern = false
  local expect_fn = false
  local expect_var_or_fn = false

  local extern_export_inline_token_tag = self.tokens.tags[extern_export_inline_token_i]
  if extern_export_inline_token_tag == "keyword_extern" then
    self:eatToken("string_literal")
    is_extern = true
    expect_var_or_fn = true
  elseif extern_export_inline_token_tag == "keyword_export" then
    expect_var_or_fn = true
  elseif
    extern_export_inline_token_tag == "keyword_inline" or
    extern_export_inline_token_tag == "keyword_noinline"
  then
    expect_fn = true
  else
    self.tok_i = self.tok_i - 1
  end

  local fn_proto = self:parseFnProto()
  if fn_proto then
    local tok_tag = self:tokenTag()
    if tok_tag == "semicolon" then
      self:nextToken()
      return fn_proto
    elseif tok_tag == "l_brace" then
      if is_extern then
        error("extern_fn_body")
      end
      local fn_decl_index = self:reserveNode("fn_decl")

      local body_block = assert(self:parseBlock())

      self:setNode(fn_decl_index, {
        tag = "fn_decl",
        main_token = self.nodes[fn_proto].main_token,
        lhs = fn_proto,
        rhs = body_block,
      })
      return fn_decl_index
    else
      error("expected_semi_or_lbrace")
    end
  end
  assert(not expect_fn, "expected_fn")

  local thread_local_token_i = self:eatToken("keyword_threadlocal")
  local var_decl = self:parseGlobalVarDecl()
  if var_decl then
    return var_decl
  end
  if thread_local_token_i then
    error("expected_var_decl")
  end
  if expect_var_or_fn then
    error("expected_var_decl_or_fn")
  end
  print(self:tokenTag())
  error("expected_pub_item")
end

function Parser:parseFnProto()
  local fn_token = self:eatToken("keyword_fn")
  if not fn_token then return false end

  -- We want the fn proto node to be before its children in the array
  local fn_proto_index = self:reserveNode("fn_proto")

  self:eatToken("identifier")
  local params = self:parseParamDeclList()
  local align_expr = self:parseByteAlign()
  local addrspace_expr = self:parseAddrSpace()
  local section_expr = self:parseLinkSection()
  local callconv_expr = self:parseCallconv()
  self:eatToken("bang")

  local return_type_expr = self:parseTypeExpr()
  assert(return_type_expr, "expected_return_type")

  if not align_expr and not section_expr and not callconv_expr and not addrspace_expr then
    self:setNode(fn_proto_index, {
      tag = "fn_proto_simple",
      main_token = fn_token,
      lhs = params,
      rhs = return_type_expr,
    })
  else
    self:setNode(fn_proto_index, {
      tag = "fn_proto",
      main_token = fn_token,
      lhs = {
        params,
        align_expr,
        addrspace_expr,
        section_expr,
        callconv_expr,
      },
      rhs = return_type_expr,
    })
  end
  return fn_proto_index
end

local VarArgsStatus = enum.fromArray{
  "none",
  "seen",
  "nonfinal",
}

function Parser:parseParamDeclList()
  self:expectToken("l_paren")
  local scratch_top = #self.scratch

  local varargs = VarArgsStatus.none
  while true do
    if self:eatToken("r_paren") then break end
    if varargs == VarArgsStatus.seen then
      varargs = VarArgsStatus.nonfinal
    end
    local param = self:expectParamDecl()
    if param then
      table.insert(self.scratch, param)
    elseif self.tokens.tags[self.tok_i - 1] == "ellipsis3" then
      if varargs == VarArgsStatus.none then
        varargs = VarArgsStatus.seen
      end
    end
    local tok_tag = self:tokenTag()
    if tok_tag == "comma" then
      self:nextToken()
    elseif tok_tag == "r_paren" then
      self:nextToken()
      break
    elseif
      tok_tag == "colon" or
      tok_tag == "r_brace" or
      tok_tag == "r_bracket"
    then
      error("expected r_paren")
    else
      error("expected_comma_after_param")
    end
  end
  if varargs == VarArgsStatus.nonfinal then
    error("varargs_nonfinal")
  end
  local params = self:sliceAndRestoreScratch(scratch_top)
  return params
end

function Parser:expectParamDecl()
  self:eatDocComments()
  local tok_tag = self:tokenTag()
  if tok_tag == "keyword_noalias" or tok_tag == "keyword_comptime" then
    self:nextToken()
  elseif tok_tag == "ellipsis3" then
    self:nextToken()
    return false
  end
  if self:tokenTag() == "identifier" and self.tokens.tags[self.tok_i + 1] == "colon" then
    self:nextToken()
    self:nextToken()
  end
  if self:tokenTag() == "keyword_anytype" then
    self:nextToken()
    return false
  else
    return self:expectTypeExpr()
  end
end


function Parser:reserveNode(tag)
  return self:addNode({
    tag = tag,
    main_token = undefined,
    lhs = undefined,
    rhs = undefined,
  })
end

function Parser:reserveNode(tag)
  return self:addNode({
    tag = tag,
    main_token = undefined,
    lhs = undefined,
    rhs = undefined,
  })
end

function Parser:parseGlobalVarDecl()
  local var_decl = self:parseVarDeclProto()
  if not var_decl then
    return false
  end

  local init_node = false
  if self:tokenTag() == "equal_equal" then
    error("wrong_equal_var_decl")
  elseif self:tokenTag() == "equal" then
    self:nextToken()
    init_node = self:expectExpr()
  end

  self.nodes[var_decl].rhs = init_node

  self:expectSemicolon("expected_semi_after_decl", false)
  return var_decl
end

function Parser:parseVarDeclProto()
  local mut_token = self:eatToken("keyword_const")
  if not mut_token then
    mut_token = self:eatToken("keyword_var")
  end
  if not mut_token then
    return false
  end

  self:expectToken("identifier")
  local type_node = false
  if self:eatToken("colon") then
    type_node = self:expectTypeExpr()
  end
  local align_node = self:parseByteAlign()
  local addrspace_node = self:parseAddrSpace()
  local section_node = self:parseLinkSection()

  if not section_node and not addrspace_node then
    if not align_node then
      return self:addNode({
        tag = "simple_var_decl",
        main_token = mut_token,
        lhs = type_node,
        rhs = false,
      })
    end

    if not type_node then
      return self:addNode({
        tag = "aligned_var_decl",
        main_token = mut_token,
        lhs = align_node,
        rhs = false,
      })
    end

    return self:addNode({
      tag = "local_var_decl",
      main_token = mut_token,
      lhs = {
        type_node,
        align_node,
      },
      rhs = false,
    })
  else
    return self:addNode({
      tag = "global_var_decl",
      main_token = mut_token,
      lhs = {
        type_node,
        align_node,
        addrspace_node,
        section_node,
      },
      rhs = false,
    })
  end
end

function Parser:parseByteAlign()
  if not self:eatToken("keyword_align") then
    return false
  end
  self:expectToken("l_paren")
  local expr = self:expectExpr()
  self:expectToken("r_paren")
  return expr
end

function Parser:parseAddrSpace()
  if not self:eatToken("keyword_addrspace") then
    return false
  end
  self:expectToken("l_paren")
  local expr = self:expectExpr()
  self:expectToken("r_paren")
  return expr
end

function Parser:parseLinkSection()
  if not self:eatToken("keyword_linksection") then
    return false
  end
  self:expectToken("l_paren")
  local expr = self:expectExpr()
  self:expectToken("r_paren")
  return expr
end

function Parser:parseCallconv()
  local callconv_token = self:eatToken("keyword_callconv")
  if not callconv_token then return false end
  self:expectToken("l_paren")
  local expr_node = self:expectExpr()
  self:expectToken("r_paren")
  return expr_node
end

function Parser:parseTypeExpr()
  local token_tag = self.tokens.tags[self.tok_i]
  if token_tag == "question_mark" then
    return self:addNode({
      tag = "optional_type",
      main_token = self:nextToken(),
      lhs = self:expectTypeExpr(),
      rhs = undefined,
    })
  elseif token_tag == "keyword_anyframe" then
    error("TODO and/or not implemented")
  elseif token_tag == "asterisk" then
    local asterisk = self:nextToken()
    local mods = self:parsePtrModifiers()
    local elem_type = self:expectTypeExpr()
    if mods.bit_range_start then
      return self:addNode({
        tag = "ptr_type_bit_range",
        main_token = asterisk,
        lhs = {
          false, -- sentinel
          mods.align_node,
          mods.addrspace_node,
          mods.bit_range_start,
          mods.bit_range_end,
        },
        rhs = elem_type,
      })
    elseif mods.addrspace_node then
      return self:addNode({
        tag = "ptr_type",
        main_token = asterisk,
        lhs = {
          false, -- sentinel
          mods.align_node,
          mods.addrspace_node,
        },
        rhs = elem_type,
      })
    else
      return self:addNode({
        tag = "ptr_type_aligned",
        main_token = asterisk,
        lhs = mods.align_node,
        rhs = elem_type,
      })
    end
  elseif token_tag == "asterisk_asterisk" then
    local asterisk = self:nextToken()
    local mods = self:parsePtrModifiers()
    local elem_type = self:expectTypeExpr()
    local inner
    if mods.bit_range_start then
      inner = self:addNode({
        tag = "ptr_type_bit_range",
        main_token = asterisk,
        lhs = {
          false, -- sentinel
          mods.align_node,
          mods.addrspace_node,
          mods.bit_range_start,
          mods.bit_range_end,
        },
        rhs = elem_type,
      })
    elseif mods.addrspace_node then
      inner = self:addNode({
        tag = "ptr_type",
        main_token = asterisk,
        lhs = {
          false, -- sentinel
          mods.align_node,
          mods.addrspace_node,
        },
        rhs = elem_type,
      })
    else
      inner = self:addNode({
        tag = "ptr_type_aligned",
        main_token = asterisk,
        lhs = mods.align_node,
        rhs = elem_type,
      })
    end
    return self:addNode({
      tag = "ptr_type_aligned",
      main_token = asterisk,
      lhs = false,
      rhs = inner,
    })
  elseif token_tag == "l_bracket" then
    local next_token_tag = self.tokens.tags[self.tok_i + 1]
    if next_token_tag == "asterisk" then
      local l_bracket = self:nextToken()
      self:nextToken()
      local sentinel = false
      local identifier_token_i = self:eatToken("identifier")
      if identifier_token_i then
        local ident_slice = self.source:sub(
          self.tokens.starts[identifier_token_i],
          self.tokens.ends[identifier_token_i]
        )
        local ident_slice_trimmed = ident_slice:match'^(%S*)'
        if ident_slice_trimmed ~= "c" then
          self.tok_i = self.tok_i - 1
        end
      elseif self:eatToken("colon") then
        sentinel = self:expectExpr()
      end
      self:expectToken("r_bracket")
      local mods = self:parsePtrModifiers()
      local elem_type = self:expectTypeExpr()
      if not mods.bit_range_start then
        if not sentinel and not mods.addrspace_node then
          return self:addNode({
            tag = "ptr_type_aligned",
            main_token = l_bracket,
            lhs = mods.align_node,
            rhs = elem_type,
          })
        elseif not mods.align_node and not mods.addrspace_node then
          return self:addNode({
            tag = "ptr_type_sentinel",
            main_token = l_bracket,
            lhs = sentinel,
            rhs = elem_type,
          })
        else
          return self:addNode({
            tag = "ptr_type",
            main_token = l_bracket,
            lhs = {
              sentinel,
              mods.align_node,
              mods.addrspace_node,
            },
            rhs = elem_type,
          })
        end
      else
        return self:addNode({
          tag = "ptr_type_bit_range",
          main_token = l_bracket,
          lhs = {
            sentinel,
            mods.align_node,
            mods.addrspace_node,
            mods.bit_range_start,
            mods.bit_range_end,
          },
          rhs = elem_type,
        })
      end
    else
      local lbracket = self:nextToken()
      local len_expr = self:parseExpr()
      local sentinel = false
      if self:eatToken("colon") then
        sentinel = self:expectExpr()
      end
      self:expectToken("r_bracket")
      if not len_expr then
        local mods = self:parsePtrModifiers()
        local elem_type = self:expectTypeExpr()
        assert(not mods.bit_range_start, "invalid_bit_range")
        if not sentinel and not mods.addrspace_node then
          return self:addNode({
            tag = "ptr_type_aligned",
            main_token = lbracket,
            lhs = mods.align_node,
            rhs = elem_type,
          })
        elseif not mods.align_node and not mods.addrspace_node then
          return self:addNode({
            tag = "ptr_type_sentinel",
            main_token = lbracket,
            lhs = sentinel,
            rhs = elem_type,
          })
        else
          return self:addNode({
            tag = "ptr_type",
            main_token = lbracket,
            lhs = { sentinel, mods.align_node, mods.addrspace_node },
            rhs = elem_type,
          })
        end
      else
        local tok_tag = self:tokenTag()
        if
          tok_tag == "keyword_align" or
          tok_tag == "keyword_const" or
          tok_tag == "keyword_volatile" or
          tok_tag == "keyword_allowzero" or
          tok_tag == "keyword_addrspace"
        then
          error("ptr_mod_on_array_child_type")
        end
        local elem_type = self:expectTypeExpr()
        if not sentinel then
          return self:addNode({
            tag = "array_type",
            main_token = lbracket,
            lhs = len_expr,
            rhs = elem_type,
          })
        else
          return self:addNode({
            tag = "array_type_sentinel",
            main_token = lbracket,
            lhs = len_expr,
            rhs = {
              sentinel,
              elem_type,
            }
          })
        end
      end
    end
  else
    return self:parseErrorUnionExpr()
  end
end

function Parser:parsePtrModifiers()
  local result = {
    align_node = false,
    addrspace_node = false,
    bit_range_start = false,
    bit_range_end = false,
  }
  local saw_const = false
  local saw_volatile = false
  local saw_allowzero = false
  while true do
    local tok_tag = self.tokens.tags[self.tok_i]
    if tok_tag == "keyword_align" then
      assert(not result.align_node, "extra_align_qualifier")
      self:nextToken()
      self:expectToken("l_paren")
      result.align_node = self:expectExpr()
      if self:eatToken("colon") then
        result.bit_range_start = self:expectExpr()
        self:expectToken("colon")
        result.bit_range_end = self:expectExpr()
      end
      self:expectToken("r_paren")
    elseif tok_tag == "keyword_const" then
      assert(not saw_const, "extra_const_qualifier")
      self:nextToken()
      saw_const = true
    elseif tok_tag == "keyword_volatile" then
      assert(not saw_volatile, "extra_volatile_qualifier")
      self:nextToken()
      saw_volatile = true
    elseif tok_tag == "keyword_allowzero" then
      assert(not saw_allowzero, "extra_allowzero_qualifier")
      self:nextToken()
      saw_allowzero = true
    elseif tok_tag == "keyword_addrspace" then
      assert(not result.addrspace_node, "extra_addrspace_qualifier")
      result.addrspace_node = self:parseAddrSpace()
    else
      return result
    end
  end
end

function Parser:parseAddrSpace()
  local addrspace_token = self:eatToken("keyword_addrspace")
  if not addrspace_token then return false end
  self:expectToken("l_paren")
  local expr_node = self:expectExpr()
  self:expectToken("r_paren")
  return expr_node
end

function Parser:parseErrorUnionExpr()
  local suffix_expr = self:parseSuffixExpr()
  if not suffix_expr then return false end
  local bang = self:eatToken("bang")
  if not bang then
    return suffix_expr
  end
  return self:addNode({
    tag = "error_union",
    main_token = bang,
    lhs = suffix_expr,
    rhs = self:expectTypeExpr(),
  })
end

function Parser:parseSuffixExpr()
  local res = self:parsePrimaryTypeExpr()
  if not res then return res end

  while true do
    local suffix_op = self:parseSuffixOp(res)
    if suffix_op then
      res = suffix_op
    else
      local lparen = self:eatToken("l_paren")
      if not lparen then
        return res
      end

      local scratch_top = #self.scratch
      -- defer shrink scratch

      while true do
        if self:eatToken("r_paren") then break end
        local param = self:expectExpr()
        table.insert(self.scratch, param)
        local token_tag = self.tokens.tags[self.tok_i]
        if token_tag == "comma" then
          self:nextToken()
        elseif token_tag == "r_paren" then
          self:nextToken()
          break
        elseif
          token_tag == "colon" or
          token_tag == "r_brace" or
          token_tag == "r_bracket"
        then
          error("expected r_paren, found "..token_tag)
        else
          error("expected_comma_after_arg")
        end
      end

      local comma = self.tokens.tags[self.tok_i - 2] == "comma"
      local params = self:sliceAndRestoreScratch(scratch_top)
      res = self:addNode({
        tag = comma and "call_comma" or "call",
        main_token = lparen,
        lhs = res,
        rhs = params,
      })
    end
  end
end

function Parser:parsePrimaryTypeExpr()
  local token_tag = self.tokens.tags[self.tok_i]
  if token_tag == "char_literal" then
    return self:addNode({
      tag = "char_literal",
      main_token = self:nextToken(),
      lhs = undefined,
      rhs = undefined,
    })
  elseif token_tag == "number_literal" then
    return self:addNode({
      tag = "number_literal",
      main_token = self:nextToken(),
      lhs = undefined,
      rhs = undefined,
    })
  elseif token_tag == "keyword_unreachable" then
    return self:addNode({
      tag = "unreachable_literal",
      main_token = self:nextToken(),
      lhs = undefined,
      rhs = undefined,
    })
  elseif token_tag == "keyword_anyframe" then
    return self:addNode({
      tag = "anyframe_literal",
      main_token = self:nextToken(),
      lhs = undefined,
      rhs = undefined,
    })
  elseif token_tag == "string_literal" then
    return self:addNode({
      tag = "string_literal",
      main_token = self:nextToken(),
      lhs = undefined,
      rhs = undefined,
    })
  elseif token_tag == "builtin" then
    return self:parseBuiltinCall()
  elseif token_tag == "keyword_fn" then
    return self:parseFnProto()
  elseif token_tag == "keyword_if" then
    return self:parseIf(Parser.expectTypeExpr)
  elseif token_tag == "keyword_switch" then
    return self:expectSwitchExpr(false)
  elseif token_tag == "keyword_extern" or token_tag == "keyword_packed" then
    self:nextToken()
    return self:parseContainerDeclAuto()
  elseif
    token_tag == "keyword_struct" or
    token_tag == "keyword_opaque" or
    token_tag == "keyword_enum" or
    token_tag == "keyword_union"
  then
    return self:parseContainerDeclAuto()
  elseif token_tag == "keyword_comptime" then
    return self:addNode({
      tag = "comptime",
      main_token = self:nextToken(),
      lhs = self:expectTypeExpr(),
      rhs = undefined,
    })
  elseif token_tag == "multiline_string_literal_line" then
    local first_line = self:nextToken()
    while self.tokens.tags[self.tok_i] == "multiline_string_literal_line" do
      self:nextToken()
    end
    return self:addNode({
      tag = "multiline_string_literal",
      main_token = first_line,
      lhs = first_line,
      rhs = self.tok_i - 1
    })
  elseif token_tag == "identifier" then
    local next_token_tag = self.tokens.tags[self.tok_i + 1]
    if next_token_tag == "colon" then
      local next_next_token_tag = self.tokens.tags[self.tok_i + 2]
      if next_next_token_tag == "keyword_inline" then
        self:nextToken()
        self:nextToken()
        self:nextToken()
        if self:tokenTag() == "keyword_for" then
          return self:parseFor(Parser.expectTypeExpr)
        elseif self:tokenTag() == "keyword_while" then
          return self:parseWhileTypeExpr()
        else
          error("expected_inlinable")
        end
      elseif next_next_token_tag == "keyword_for" then
        self:nextToken()
        self:nextToken()
        return self:parseFor(Parser.expectTypeExpr)
      elseif next_next_token_tag == "keyword_while" then
        self:nextToken()
        self:nextToken()
        return self:parseWhileTypeExpr()
      elseif next_next_token_tag == "keyword_switch" then
        self:nextToken()
        self:nextToken()
        return self:expectSwitchExpr(true)
      elseif next_next_token_tag == "l_brace" then
        self:nextToken()
        self:nextToken()
        return self:parseBlock()
      else
        return self:addNode({
          tag = "identifier",
          main_token = self:nextToken(),
          lhs = undefined,
          rhs = undefined,
        })
      end
    else
      return self:addNode({
        tag = "identifier",
        main_token = self:nextToken(),
        lhs = undefined,
        rhs = undefined,
      })
    end
  elseif token_tag == "keyword_inline" then
    self:nextToken()
    if self:tokenTag() == "keyword_for" then
      return self:parseFor(Parser.expectTypeExpr)
    elseif self:tokenTag() == "keyword_while" then
      return self:parseWhileTypeExpr()
    else
      error("expected_inlinable")
    end
  elseif token_tag == "keyword_for" then
    return self:parseFor(Parser.expectTypeExpr)
  elseif token_tag == "keyword_while" then
    return self:parseWhileTypeExpr()
  elseif token_tag == "period" then
    local next_token_tag = self.tokens.tags[self.tok_i + 1]
    if next_token_tag == "identifier" then
      self:nextToken() -- dot
      return self:addNode({
        tag = "enum_literal",
        lhs = undefined,
        rhs = undefined,
        main_token = self:nextToken(), -- identifier
      })
    elseif next_token_tag == "l_brace" then
      local lbrace = self.tok_i + 1
      self.tok_i = lbrace + 1

      local scratch_top = #self.scratch
      local field_init = self:parseFieldInit()
      if field_init then
        table.insert(self.scratch, field_init)
        while true do
          local tok_tag = self:tokenTag()
          if tok_tag == "comma" then
            self:nextToken()
          elseif tok_tag == "r_brace" then
            self:nextToken()
            break
          elseif
            tok_tag == "colon" or
            tok_tag == "r_paren" or
            tok_tag == "r_bracket"
          then
            error("expected r_brace")
          else
            error("expected_comma_after_initializer")
          end
          if self:eatToken("r_brace") then break end
          local next_init = self:expectFieldInit()
          table.insert(self.scratch, next_init)
        end

        local comma = self.tokens.tags[self.tok_i - 2] == "comma"
        local inits = self:sliceAndRestoreScratch(scratch_top)
        assert(#inits > 0)
        return self:addNode({
          tag = comma and "struct_init_dot_comma" or "struct_init_dot",
          main_token = lbrace,
          lhs = inits,
          rhs = undefined,
        })
      end

      while true do
        if self:eatToken("r_brace") then break end
        local elem_init = self:expectExpr()
        table.insert(self.scratch, elem_init)
        local tok_tag = self:tokenTag()
        if tok_tag == "comma" then
          self:nextToken()
        elseif tok_tag == "r_brace" then
          self:nextToken()
          break
        elseif
          tok_tag == "colon" or
          tok_tag == "r_paren" or
          tok_tag == "r_bracket"
        then
          error("expected r_brace")
        else
          error("expected_comma_after_initializer")
        end
      end

      local comma = self.tokens.tags[self.tok_i - 2] == "comma"
      local inits = self:sliceAndRestoreScratch(scratch_top)
      local tag = "struct_init_dot"
      if #inits > 0 then
        tag = comma and "array_init_dot_comma" or "array_init_dot"
      end
      return self:addNode({
        tag = tag,
        main_token = lbrace,
        lhs = inits,
        rhs = undefined,
      })
    else
      return false
    end
  elseif token_tag == "keyword_error" then
    local next_token_tag = self.tokens.tags[self.tok_i + 1]
    if next_token_tag == "l_brace" then
      local error_token = self.tok_i
      self.tok_i = self.tok_i + 2
      while true do
        if self:eatToken("r_brace") then break end
        self:eatDocComments()
        self:expectToken("identifier")
        local tok_tag = self:tokenTag()
        if tok_tag == "comma" then
          self:nextToken()
        elseif tok_tag == "r_brace" then
          self:nextToken()
          break
        elseif
          tok_tag == "colon" or
          tok_tag == "r_paren" or
          tok_tag == "r_bracket"
        then
          error("expected r_brace")
        else
          error("expected_comma_after_field")
        end
      end
      return self:addNode({
        tag = "error_set_decl",
        main_token = error_token,
        lhs = error_token + 1, -- lbrace
        rhs = self.tok_i - 1, -- rbrace
      })
    else
      local main_token = self:nextToken()
      local period = self:eatToken("period")
      if not period then error("expected period") end
      local identifier = self:eatToken("identifier")
      if not identifier then error("expected identifier") end
      return self:addNode({
        tag = "error_value",
        main_token = main_token,
        lhs = undefined,
        rhs = undefined,
      })
    end
  elseif token_tag == "l_paren" then
    return self:addNode({
      tag = "grouped_expression",
      main_token = self:nextToken(),
      lhs = self:expectExpr(),
      rhs = self:expectToken("r_paren"),
    })
  else
    return false
  end
end

function Parser:parseFor(bodyParseFn)
  local for_token = self:eatToken("keyword_for")
  if not for_token then return false end

  local scratch_top = #self.scratch
  local inputs = self:forPrefix()

  local then_expr = bodyParseFn(self)
  local has_else = false
  if self:eatToken("keyword_else") then
    table.insert(self.scratch, then_expr)
    local else_expr = bodyParseFn(self)
    table.insert(self.scratch, else_expr)
    has_else = true
  elseif inputs == 1 then
    local lhs = self.scratch[scratch_top + 1]
    self:restoreScratch(scratch_top)
    return self:addNode({
      tag = "for_simple",
      main_token = for_token,
      lhs = lhs,
      rhs = then_expr,
    })
  else
    table.insert(self.scratch, then_expr)
  end

  local components = self:sliceAndRestoreScratch(scratch_top)
  if not has_else then
    table.insert(components, false)
  end
  assert(#components == inputs + 2)
  return self:addNode({
    tag = "for",
    main_token = for_token,
    lhs = components,
    rhs = undefined,
  })
end

function Parser:expectFieldInit()
  if
    self.tokens.tags[self.tok_i] ~= "period" or
    self.tokens.tags[self.tok_i + 1] ~= "identifier" or
    self.tokens.tags[self.tok_i + 2] ~= "equal"
  then
    error("expected_initializer")
  end

  self.tok_i = self.tok_i + 3
  return self:expectExpr()
end

function Parser:parsePtrIndexPayload()
  if not self:eatToken("pipe") then return false end
  self:eatToken("asterisk")
  local identifier = self:expectToken("identifier")
  if self:eatToken("comma") then
    self:expectToken("identifier")
  end
  self:expectToken("pipe")
  return identifier
end

function Parser:parseBuiltinCall()
  local builtin_token = self:assertToken("builtin")
  assert(self:eatToken("l_paren"), "expected_param_list")
  local scratch_top = #self.scratch

  while true do
    if self:eatToken("r_paren") then break end
    local param = self:expectExpr()
    table.insert(self.scratch, param)
    if self:tokenTag() == "comma" then
      self:nextToken()
    elseif self:tokenTag() == "r_paren" then
      self:nextToken()
      break
    else
      error("expected_comma_after_arg")
    end
  end

  local comma = self.tokens.tags[self.tok_i - 2] == "comma"
  local params = self:sliceAndRestoreScratch(scratch_top)

  return self:addNode({
    tag = comma and "builtin_call_comma" or "builtin_call",
    main_token = builtin_token,
    lhs = params,
    rhs = undefined,
  })
end

function Parser:parseContainerDeclAuto()
  local main_token = self:nextToken()
  local arg_expr
  local main_token_tag = self.tokens.tags[main_token]
  if main_token_tag == "keyword_opaque" then
    arg_expr = false
  elseif main_token_tag == "keyword_struct" or main_token_tag == "keyword_enum" then
    if self:eatToken("l_paren") then
      local expr = self:expectExpr()
      self:expectToken("r_paren")
      arg_expr = expr
    else
      arg_expr = false
    end
  elseif main_token_tag == "keyword_union" then
    if self:eatToken("l_paren") then
      if self:eatToken("keyword_enum") then
        if self:eatToken("l_paren") then
          local enum_tag_expr = self:expectExpr()
          self:expectToken("r_paren")
          self:expectToken("r_paren")

          self:expectToken("l_brace")
          local members = self:parseContainerMembers()
          self:expectToken("r_brace")
          return self:addNode({
            tag = members.trailing and "tagged_union_enum_tag_trailing" or "tagged_union_enum_tag",
            main_token = main_token,
            lhs = enum_tag_expr,
            rhs = members.list,
          })
        else
          self:expectToken("r_paren")

          self:expectToken("l_brace")
          local members = self:parseContainerMembers()
          self:expectToken("r_brace")
          return self:addNode({
            tag = members.trailing and "tagged_union_trailing" or "tagged_union",
            main_token = main_token,
            lhs = members.list,
            rhs = undefined,
          })
        end
      else
        local expr = self:expectExpr()
        self:expectToken("r_paren")
        arg_expr = expr
      end
    else
      arg_expr = false
    end
  else
    self.tok_i = self.tok_i - 1
    error("expected_container")
  end

  self:expectToken("l_brace")
  local members = self:parseContainerMembers()
  self:expectToken("r_brace")
  if not arg_expr then
    return self:addNode({
      tag = members.trailing and "container_decl_trailing" or "container_decl",
      main_token = main_token,
      lhs = members.list,
      rhs = undefined,
    })
  else
    return self:addNode({
      tag = members.trailing and "container_decl_arg_trailing" or "container_decl_arg",
      main_token = main_token,
      lhs = arg_expr,
      rhs = members.list,
    })
  end
end

function Parser:parseSuffixOp(lhs)
  local token_tag = self.tokens.tags[self.tok_i]
  if token_tag == "l_bracket" then
    local lbracket = self:nextToken()
    local index_expr = self:expectExpr()

    if self:eatToken("ellipsis2") then
      local end_expr = self:parseExpr()
      if self:eatToken("colon") then
        local sentinel = self:expectExpr()
        self:expectToken("r_bracket")
        return self:addNode({
          tag = "slice_sentinel",
          main_token = lbracket,
          lhs = lhs,
          rhs = {
            index_expr,
            end_expr,
            sentinel,
          },
        })
      end
      self:expectToken("r_bracket")
      if not end_expr then
        return self:addNode({
          tag = "slice_open",
          main_token = lbracket,
          lhs = lhs,
          rhs = index_expr,
        })
      end
      return self:addNode({
        tag = "slice",
        main_token = lbracket,
        lhs = lhs,
        rhs = {
          index_expr,
          end_expr,
        },
      })
    end
    self:expectToken("r_bracket")
    return self:addNode({
      tag = "array_access",
      main_token = lbracket,
      lhs = lhs,
      rhs = index_expr,
    })
  elseif token_tag == "period_asterisk" then
    return self:addNode({
      tag = "deref",
      main_token = self:nextToken(),
      lhs = lhs,
      rhs = undefined,
    })
  elseif token_tag == "invalid_periodasterisks" then
    error("asterisk_after_ptr_deref")
  elseif token_tag == "period" then
    local next_tag = self.tokens.tags[self.tok_i + 1]
    if next_tag == "identifier" then
      return self:addNode({
        tag = "field_access",
        main_token = self:nextToken(),
        lhs = lhs,
        rhs = self:nextToken(),
      })
    elseif next_tag == "question_mark" then
      return self:addNode({
        tag = "unwrap_optional",
        main_token = self:nextToken(),
        lhs = lhs,
        rhs = self:nextToken(),
      })
    elseif next_tag == "l_brace" then
      return false
    else
      error("expected_suffix_op")
    end
  else
    return false
  end
end

function Parser:expectPrefixExpr()
  return assert(self:parsePrefixExpr(), "expected_prefix_expr")
end

function Parser:expectExpr()
  return assert(self:parseExpr(), "expected_expr")
end

function Parser:expectTypeExpr()
  return assert(self:parseTypeExpr(), "expected_type_expr")
end

function Parser:parseExpr()
  return self:parseExprPrecedence(0)
end

local operTableDefault = { prec = -1, tag = false, left_assoc = true }
local operTable = {
    keyword_or = { prec = 10, tag = "bool_or", left_assoc = true },

    keyword_and = { prec = 20, tag = "bool_and", left_assoc = true },

    equal_equal = { prec = 30, tag = "equal_equal", left_assoc = false },
    bang_equal = { prec = 30, tag = "bang_equal", left_assoc = false },
    angle_bracket_left = { prec = 30, tag = "less_than", left_assoc = false },
    angle_bracket_right = { prec = 30, tag = "greater_than", left_assoc = false },
    angle_bracket_left_equal = { prec = 30, tag = "less_or_equal", left_assoc = false },
    angle_bracket_right_equal = { prec = 30, tag = "greater_or_equal", left_assoc = false },

    ampersand = { prec = 40, tag = "bit_and", left_assoc = true },
    caret = { prec = 40, tag = "bit_xor", left_assoc = true },
    pipe = { prec = 40, tag = "bit_or", left_assoc = true },
    keyword_orelse = { prec = 40, tag = "orelse", left_assoc = true },
    keyword_catch = { prec = 40, tag = "catch", left_assoc = true },

    angle_bracket_angle_bracket_left = { prec = 50, tag = "shl", left_assoc = true },
    angle_bracket_angle_bracket_left_pipe = { prec = 50, tag = "shl_sat", left_assoc = true },
    angle_bracket_angle_bracket_right = { prec = 50, tag = "shr", left_assoc = true },

    plus = { prec = 60, tag = "add", left_assoc = true },
    minus = { prec = 60, tag = "sub", left_assoc = true },
    plus_plus = { prec = 60, tag = "array_cat", left_assoc = true },
    plus_percent = { prec = 60, tag = "add_wrap", left_assoc = true },
    minus_percent = { prec = 60, tag = "sub_wrap", left_assoc = true },
    plus_pipe = { prec = 60, tag = "add_sat", left_assoc = true },
    minus_pipe = { prec = 60, tag = "sub_sat", left_assoc = true },

    pipe_pipe = { prec = 70, tag = "merge_error_sets", left_assoc = true },
    asterisk = { prec = 70, tag = "mul", left_assoc = true },
    slash = { prec = 70, tag = "div", left_assoc = true },
    percent = { prec = 70, tag = "mod", left_assoc = true },
    asterisk_asterisk = { prec = 70, tag = "array_mult", left_assoc = true },
    asterisk_percent = { prec = 70, tag = "mul_wrap", left_assoc = true },
    asterisk_pipe = { prec = 70, tag = "mul_sat", left_assoc = true },
}
function Parser:parseExprPrecedence(min_prec)
  assert(min_prec >= 0)
  local node = self:parsePrefixExpr()
  if not node then
    return false
  end

  local banned_prec = -1

  while true do
    local tok_tag = self.tokens.tags[self.tok_i]
    local info = operTable[tok_tag] or operTableDefault
    if info.prec < min_prec then
      break
    end
    if info.prec == banned_prec then
      error("chained_comparison_operators")
    end

    local oper_token = self:nextToken()
    if tok_tag == "keyword_catch" then
      self:parsePayload()
    end
    local rhs = self:parseExprPrecedence(info.prec + 1)
    if not rhs then
      error("expected_expr")
    end

    do
      local tok_len = #(token_lexemes[tok_tag])
      local char_before = self.source:byte(self.tokens.starts[oper_token] - 1)
      local char_after = self.source:byte(self.tokens.starts[oper_token] + tok_len)
      if tok_tag == "ampersand" and char_after == ('&'):byte() then
        error("invalid_ampersand_ampersand")
      elseif isWhitespace(char_before) ~= isWhitespace(char_after) then
        error("mismatched_binary_op_whitespace")
      end
    end

    node = self:addNode({
      tag = info.tag,
      main_token = oper_token,
      lhs = node,
      rhs = rhs,
    })

    if not info.left_assoc then
      banned_prec = info.prec
    end
  end

  return node
end

local prefix_expr_tag_mapping = {
  bang = "bool_not",
  minus = "negation",
  tilde = "bit_not",
  minus_percent = "negation_wrap",
  ampersand = "address_of",
  keyword_try = "try",
}
function Parser:parsePrefixExpr()
  local tok_tag = self:tokenTag()
  local node_tag = prefix_expr_tag_mapping[tok_tag]
  if not node_tag then
    return self:parsePrimaryExpr()
  end
  return self:addNode({
    tag = node_tag,
    main_token = self:nextToken(),
    lhs = self:expectPrefixExpr(),
    rhs = undefined,
  })
end

function Parser:parsePrimaryExpr()
  local tok_tag = self:tokenTag()
  if tok_tag == "keyword_asm" then
    return self:expectAsmExpr()
  elseif tok_tag == "keyword_if" then
    return self:parseIfExpr()
  elseif tok_tag == "keyword_break" then
    return self:addNode({
      tag = "break",
      main_token = self:nextToken(),
      lhs = self:parseBreakLabel(),
      rhs = self:parseExpr(),
    })
  elseif tok_tag == "keyword_continue" then
    return self:addNode({
      tag = "continue",
      main_token = self:nextToken(),
      lhs = self:parseBreakLabel(),
      rhs = self:parseExpr(),
    })
  elseif tok_tag == "keyword_comptime" then
    return self:addNode({
      tag = "comptime",
      main_token = self:nextToken(),
      lhs = self:expectExpr(),
      rhs = undefined,
    })
  elseif tok_tag == "keyword_nosuspend" then
    return self:addNode({
      tag = "nosuspend",
      main_token = self:nextToken(),
      lhs = self:expectExpr(),
      rhs = undefined,
    })
  elseif tok_tag == "keyword_resume" then
    error("TODO and/or unimplemented")
  elseif tok_tag == "keyword_return" then
    return self:addNode({
      tag = "return",
      main_token = self:nextToken(),
      lhs = self:parseExpr(),
      rhs = undefined,
    })
  elseif tok_tag == "identifier" then
    if self.tokens.tags[self.tok_i + 1] == "colon" then
      local label_tok_tag = self.tokens.tags[self.tok_i + 2]
      if label_tok_tag == "keyword_inline" then
        self:nextToken()
        self:nextToken()
        self:nextToken()
        if self:tokenTag() == "keyword_for" then
          return self:parseFor(Parser.expectExpr)
        elseif self:tokenTag() == "keyword_while" then
          return self:parseWhileExpr()
        else
          error("expected_inlinable")
        end
      elseif label_tok_tag == "keyword_for" then
        self:nextToken()
        self:nextToken()
        return self:parseFor(Parser.expectExpr)
      elseif label_tok_tag == "keyword_while" then
        self:nextToken()
        self:nextToken()
        return self:parseWhileExpr()
      elseif label_tok_tag == "l_brace" then
        self:nextToken()
        self:nextToken()
        return self:parseBlock()
      else
        return self:parseCurlySuffixExpr()
      end
    else
      return self:parseCurlySuffixExpr()
    end
  elseif tok_tag == "keyword_inline" then
    self:nextToken()
    if self:tokenTag() == "keyword_for" then
      return self:parseFor(Parser.expectExpr)
    elseif self:tokenTag() == "keyword_while" then
      return self:parseWhileExpr()
    else
      error("expected_inlinable")
    end
  elseif tok_tag == "keyword_for" then
    return self:parseFor(Parser.expectExpr)
  elseif tok_tag == "keyword_while" then
    return self:parseWhileExpr()
  elseif tok_tag == "l_brace" then
    return self:parseBlock()
  else
    return self:parseCurlySuffixExpr()
  end
end

function Parser:expectAsmExpr()
  local asm_token = self:assertToken("keyword_asm")
  self:eatToken("keyword_volatile")
  self:expectToken("l_paren")
  local template = self:expectExpr()

  do
    local rparen = self:eatToken("r_paren")
    if rparen then
      return self:addNode({
        tag = "asm_simple",
        main_token = asm_token,
        lhs = template,
        rhs = rparen,
      })
    end
  end

  self:expectToken("colon")

  local scratch_top = #self.scratch

  while true do
    local output_item = self:parseAsmOutputItem()
    if not output_item then break end
    table.insert(self.scratch, output_item)
    local tok_tag = self:tokenTag()
    if tok_tag == "comma" then
      self:nextToken()
    elseif
      tok_tag == "colon" or
      tok_tag == "r_paren" or
      tok_tag == "r_brace" or
      tok_tag == "r_bracket"
    then
      break
    else
      error("expected comma")
    end
  end

  local clobbers = false
  if self:eatToken("colon") then
    while true do
      local input_item = self:parseAsmInputItem()
      if not input_item then break end
      table.insert(self.scratch, input_item)
      local tok_tag = self:tokenTag()
      if tok_tag == "comma" then
        self:nextToken()
      elseif
        tok_tag == "colon" or
        tok_tag == "r_paren" or
        tok_tag == "r_brace" or
        tok_tag == "r_bracket"
      then
        break
      else
        error("expected comma")
      end
    end
    if self:eatToken("colon") then
      clobbers = self:expectExpr()
    end
  end

  local rparen = self:expectToken("r_paren")
  local items = self:sliceAndRestoreScratch(scratch_top)
  return self:addNode({
    tag = "asm",
    main_token = asm_token,
    lhs = template,
    rhs = {
      items,
      clobbers,
      rparen,
    },
  })
end

function Parser:parseAsmOutputItem()
  local lbracket = self:eatToken("l_bracket")
  if not lbracket then return false end
  local identifier = self:expectToken("identifier")
  self:expectToken("r_bracket")
  self:expectToken("string_literal")
  self:expectToken("l_paren")

  local type_expr = false
  if self:eatToken("arrow") then
    type_expr = self:expectTypeExpr()
  else
    self:expectToken("identifier")
  end
  local rparen = self:expectToken("r_paren")
  return self:addNode({
    tag = "asm_output",
    main_token = identifier,
    lhs = type_expr,
    rhs = rparen,
  })
end

function Parser:parseAsmInputItem()
  local lbracket = self:eatToken("l_bracket")
  if not lbracket then return false end
  local identifier = self:expectToken("identifier")
  self:expectToken("r_bracket")
  self:expectToken("string_literal")
  self:expectToken("l_paren")
  local expr = self:expectExpr()
  local rparen = self:expectToken("r_paren")
  return self:addNode({
    tag = "asm_input",
    main_token = identifier,
    lhs = expr,
    rhs = rparen,
  })
end

function Parser:parseIfExpr()
  return self:parseIf(Parser.expectExpr)
end

function Parser:parseIf(bodyParseFn)
  local if_token = self:eatToken("keyword_if")
  if not if_token then return false end

  self:expectToken("l_paren")
  local condition = self:expectExpr()
  self:expectToken("r_paren")
  self:parsePtrPayload()

  local then_expr = assert(bodyParseFn(self))

  local else_token = self:eatToken("keyword_else")
  if not else_token then
    return self:addNode({
      tag = "if_simple",
      main_token = if_token,
      lhs = condition,
      rhs = then_expr,
    })
  end
  self:parsePayload()
  local else_expr = assert(bodyParseFn(self))
  return self:addNode({
    tag = "if",
    main_token = if_token,
    lhs = condition,
    rhs = {
      then_expr,
      else_expr,
    }
  })
end

function Parser:parseBreakLabel()
  local colon_tok = self:eatToken("colon")
  if not colon_tok then
    return false
  end
  return self:expectToken("identifier")
end

function Parser:expectBlockExprStatement()
  return assert(self:parseBlockExprStatement(), "expected_block_or_expr")
end

function Parser:parseBlockExprStatement()
  local block_expr = self:parseBlockExpr()
  if block_expr then return block_expr end

  local assign_expr = self:parseAssignExpr()
  if assign_expr then
    self:expectSemicolon("expected_semi_after_stmt", true)
    return assign_expr
  end

  return false
end

function Parser:parseBlockExpr()
  local tok_tag = self:tokenTag()
  if tok_tag == "identifier" then
    if
      self.tokens.tags[self.tok_i + 1] == "colon" and
      self.tokens.tags[self.tok_i + 2] == "l_brace"
    then
      self:nextToken()
      self:nextToken()
      return self:parseBlock()
    else
      return false
    end
  elseif tok_tag == "l_brace" then
    return self:parseBlock()
  else
    return false
  end
end

function Parser:parseBlock()
  local lbrace = self:eatToken("l_brace")
  if not lbrace then return false end
  local scratch_top = #self.scratch
  -- defer shrink scratch

  while true do
    if self:tokenTag() == "r_brace" then break end
    local top_before = #self.scratch
    local statement = self:expectStatement(true)
    assert(top_before == #self.scratch)
    if not statement then break end
    table.insert(self.scratch, statement)
  end

  self:expectToken("r_brace")
  local semicolon = self.tokens.tags[self.tok_i - 2] == "semicolon"
  local statements = self:sliceAndRestoreScratch(scratch_top)
  return self:addNode({
    tag = semicolon and "block_semicolon" or "block",
    main_token = lbrace,
    lhs = statements,
    rhs = undefined,
  })
end

function Parser:expectStatement(allow_defer_var)
  local comptime_token = self:eatToken("keyword_comptime")
  if comptime_token then
    local block_expr = self:parseBlockExpr()
    if block_expr then
      return self:addNode({
        tag = "comptime",
        main_token = comptime_token,
        lhs = block_expr,
        rhs = undefined,
      })
    end

    if allow_defer_var then
      return self:expectVarDeclExprStatement(comptime_token)
    else
      local assign = self:expectAssignExpr()
      self:expectSemicolon("expected_semi_after_stmt", true)
      return self:addNode({
        tag = "comptime",
        main_token = comptime_token,
        lhs = assign,
        rhs = undefined,
      })
    end
  end

  local tok_tag = self:tokenTag()
  if tok_tag == "keyword_nosuspend" then
    return self:addNode({
      tag = "nosuspend",
      main_token = self:nextToken(),
      lhs = self:expectBlockExprStatement(),
      rhs = undefined,
    })
  elseif tok_tag == "keyword_suspend" then
    error("TODO and/or unimplemented")
  elseif tok_tag == "keyword_defer" then
    if allow_defer_var then
      return self:addNode({
        tag = "defer",
        main_token = self:nextToken(),
        lhs = undefined,
        rhs = self:expectBlockExprStatement(),
      })
    end
  elseif tok_tag == "keyword_errdefer" then
    if allow_defer_var then
      return self:addNode({
        tag = "errdefer",
        main_token = self:nextToken(),
        lhs = self:parsePayload(),
        rhs = self:expectBlockExprStatement(),
      })
    end
  elseif tok_tag == "keyword_if" then
    return self:expectIfStatement()
  elseif
    tok_tag == "keyword_enum" or
    tok_tag == "keyword_struct" or
    tok_tag == "keyword_union"
  then
    -- local identifier = self.tok_i + 1
    -- TODO maybe parseCStyleContainer
  end

  local labeled_statement = self:parseLabeledStatement()
  if labeled_statement then
    return labeled_statement
  end

  if allow_defer_var then
    return self:expectVarDeclExprStatement(nil)
  else
    local assign = self:expectAssignExpr();
    self:expectSemicolon("expected_semi_after_stmt", true)
    return assign
  end
end

function Parser:expectIfStatement()
  local if_token = self:assertToken("keyword_if")
  self:expectToken("l_paren")
  local condition = self:expectExpr()
  self:expectToken("r_paren")
  self:parsePtrPayload()

  local else_required = false
  local then_expr
  do
    local block_expr = self:parseBlockExpr()
    if block_expr then
      then_expr = block_expr
    else
      local assign_expr = assert(self:parseAssignExpr(), "expected_block_or_assignment")
      if self:eatToken("semicolon") then
        return self:addNode({
          tag = "if_simple",
          main_token = if_token,
          lhs = condition,
          rhs = assign_expr,
        })
      end
      else_required = true
      then_expr = assign_expr
    end
  end

  local else_token = self:eatToken("keyword_else")
  if not else_token then
    assert(not else_required, "expected_semi_or_else")
    return self:addNode({
      tag = "if_simple",
      main_token = if_token,
      lhs = condition,
      rhs = then_expr,
    })
  end

  self:parsePayload()
  local else_expr = self:expectStatement(false)
  return self:addNode({
    tag = "if",
    main_token = if_token,
    lhs = condition,
    rhs = {
      then_expr,
      else_expr,
    },
  })
end

function Parser:parsePayload()
  local pipe_token = self:eatToken("pipe")
  if not pipe_token then return false end
  local identifier = self:expectToken("identifier")
  self:expectToken("pipe")
  return identifier
end

function Parser:parsePtrPayload()
  local pipe_token = self:eatToken("pipe")
  if not pipe_token then return false end
  self:eatToken("asterisk")
  local identifier = self:expectToken("identifier")
  self:expectToken("pipe")
  return identifier
end

function Parser:expectVarDeclExprStatement(comptime_token)
  local scratch_top = #self.scratch
  -- defer restore scratch

  while true do
    local var_decl_proto = self:parseVarDeclProto()
    if var_decl_proto then
      table.insert(self.scratch, var_decl_proto)
    else
      local expr = self:parseExpr()
      if not expr then
        if #self.scratch == scratch_top then
          error("expected_statement")
        else
          error("expected_expr_or_var_decl")
        end
      end
      table.insert(self.scratch, expr)
    end
    if not self:eatToken("comma") then break end
  end

  local lhs_count = #self.scratch - scratch_top
  assert(lhs_count > 0)

  local equal_token = self:eatToken("equal")
  if not equal_token then
    if lhs_count > 1 then
      local equal_equal_token = self:eatToken("equal_equal")
      if equal_equal_token then
        error("wrong_equal_var_decl")
      end
      error("expected equal")
    end
    local lhs = self.scratch[scratch_top + 1]
    local lhs_tag = self.nodes[lhs].tag
    if
      lhs_tag == "global_var_decl" or
      lhs_tag == "local_var_decl" or
      lhs_tag == "simple_var_decl" or
      lhs_tag == "aligned_var_decl"
    then
      local equal_equal_token = self:eatToken("equal_equal")
      if equal_equal_token then
        error("wrong_equal_var_decl")
      end
      error("expected equal")
    end

    local expr = self:finishAssignExpr(lhs)
    self:expectSemicolon("expected_semi_after_stmt", true)

    self:restoreScratch(scratch_top)
    if comptime_token then
      return self:addNode({
        tag = "comptime",
        main_token = comptime_token,
        lhs = expr,
        rhs = undefined,
      })
    else
      return expr
    end
  end

  local rhs = self:expectExpr()
  self:expectSemicolon("expected_semi_after_stmt", true)

  if lhs_count == 1 then
    local lhs = self.scratch[scratch_top + 1]
    local lhs_tag = self.nodes[lhs].tag
    if
      lhs_tag == "global_var_decl" or
      lhs_tag == "local_var_decl" or
      lhs_tag == "simple_var_decl" or
      lhs_tag == "aligned_var_decl"
    then
      self.nodes[lhs].rhs = rhs
      self:restoreScratch(scratch_top)
      return lhs
    end
    local expr = self:addNode({
      tag = "assign",
      main_token = equal_token,
      lhs = lhs,
      rhs = rhs,
    })

    self:restoreScratch(scratch_top)
    if comptime_token then
      return self:addNode({
        tag = "comptime",
        main_token = comptime_token,
        lhs = expr,
        rhs = undefined,
      })
    else
      return expr
    end
  end

  local vals = self:sliceAndRestoreScratch(scratch_top)
  return self:addNode({
    tag = "assign_destructure",
    main_token = equal_token,
    lhs = vals,
    rhs = rhs,
  })
end

function Parser:parseLabeledStatement()
  local label_token = self:parseBlockLabel()
  local block = self:parseBlock()
  if block then return block end

  local loop_stmt = self:parseLoopStatement()
  if loop_stmt then return loop_stmt end

  local switch_expr = self:parseSwitchExpr(label_token)
  if switch_expr then return switch_expr end

  if label_token then
    local node = self:parseTypeExpr()
    if node then
      local a = self:parseByteAlign()
      local b = self:parseAddrSpace()
      local c = self:parseLinkSection()
      local d = false
      if self:eatToken("equal") then
        d = self:expectExpr()
      end
      if a or b or c or d then
        error("expected_var_const")
      end
    end
    error("expected_labelable")
  end

  return false
end

function Parser:parseBlockLabel()
  if
    self:tokenTag() == "identifier" and
    self.tokens.tags[self.tok_i + 1] == "colon"
  then
    local identifier = self.tok_i
    self:nextToken()
    self:nextToken()
    return identifier
  end

  return false
end

function Parser:parseLoopStatement()
  local inline_token = self:eatToken("keyword_inline")

  local for_statement = self:parseForStatement()
  if for_statement then return for_statement end

  local while_statement = self:parseWhileStatement()
  if while_statement then return while_statement end

  if not inline_token then return false end

  error("expected_inlinable")
end

function Parser:forPrefix()
  local scratch_top = #self.scratch
  self:expectToken("l_paren")

  while true do
    local input = self:expectExpr()
    local ellipsis_token = self:eatToken("ellipsis2")
    if ellipsis_token then
      input = self:addNode({
        tag = "for_range",
        main_token = ellipsis_token,
        lhs = input,
        rhs = self:parseExpr(),
      })
    end

    table.insert(self.scratch, input)
    local tok_tag = self:tokenTag()
    if tok_tag == "comma" then
      self:nextToken()
    elseif tok_tag == "r_paren" then
      self:nextToken()
      break
    elseif
      tok_tag == "colon" or
      tok_tag == "r_brace" or
      tok_tag == "r_bracket"
    then
      error("expected r_paren")
    else
      error("expected_comma_after_for_operand")
    end
    if self:eatToken("r_paren") then break end
  end
  local inputs = #self.scratch - scratch_top

  local pipe_token = self:eatToken("pipe")
  if not pipe_token then
    error("expected_loop_payload")
  end

  local captures = 0
  while true do
    self:eatToken("asterisk")
    self:expectToken("identifier")
    captures = captures + 1
    if captures > inputs then
      error("extra_for_capture")
    end
    local tok_tag = self:tokenTag()
    if tok_tag == "comma" then
      self:nextToken()
    elseif tok_tag == "pipe" then
      self:nextToken()
      break
    else
      error("expected_comma_after_capture")
    end
    if self:eatToken("pipe") then break end
  end

  if captures < inputs then
    error("for_input_not_captured")
  end

  return inputs
end

function Parser:parseForStatement()
  local for_token = self:eatToken("keyword_for")
  if not for_token then return false end

  local scratch_top = #self.scratch
  local inputs = self:forPrefix()

  local else_required = false
  local seen_semicolon = false

  local then_expr
  do
    local block_expr = self:parseBlockExpr()
    if block_expr then
      then_expr = block_expr
    else
      local assign_expr = assert(self:parseAssignExpr(), "expected_block_or_assignment")
      if self:eatToken("semicolon") then
        seen_semicolon = true
        then_expr = assign_expr
      else
        else_required = true
        then_expr = assign_expr
      end
    end
  end

  local has_else = false
  if not seen_semicolon and self:eatToken("keyword_else") then
    table.insert(self.scratch, then_expr)
    local else_stmt = self:expectStatement(false)
    table.insert(self.scratch, else_stmt)
    has_else = true
  elseif inputs == 1 then
    if else_required then error("expected_semi_or_else") end
    local lhs = self.scratch[scratch_top + 1]
    self:restoreScratch(scratch_top)
    return self:addNode({
      tag = "for_simple",
      main_token = for_token,
      lhs = lhs,
      rhs = then_expr,
    })
  else
    if else_required then error("expectd_semi_or_else") end
    table.insert(self.scratch, then_expr)
  end
  local components = self:sliceAndRestoreScratch(scratch_top)
  if not has_else then
    table.insert(components, false)
  end
  assert(#components == inputs + 2)
  return self:addNode({
    tag = "for",
    main_token = for_token,
    lhs = components,
    rhs = undefined,
  })
end

function Parser:parseWhileStatement()
  local while_token = self:eatToken("keyword_while")
  if not while_token then return false end
  self:expectToken("l_paren")
  local condition = self:expectExpr()
  self:expectToken("r_paren")
  self:parsePtrPayload()
  local cont_expr = self:parseWhileContinueExpr()

  local else_required = false
  local then_expr
  do
    local block_expr = self:parseBlockExpr()
    if block_expr then
      then_expr = block_expr
    else
      local assign_expr = assert(self:parseAssignExpr(), "expected_block_or_assignment")
      if self:eatToken("semicolon") then
        if not cont_expr then
          return self:addNode({
            tag = "while_simple",
            main_token = while_token,
            lhs = condition,
            rhs = assign_expr,
          })
        else
          return self:addNode({
            tag = "while_cont",
            main_token = while_token,
            lhs = condition,
            rhs = {
              cont_expr,
              assign_expr,
            }
          })
        end
      end
      else_required = true
      then_expr = assign_expr
    end
  end

  local else_token = self:eatToken("keyword_else")
  if not else_token then
    assert(not else_required, "expected_semi_or_else")
    if not cont_expr then
      return self:addNode({
        tag = "while_simple",
        main_token = while_token,
        lhs = condition,
        rhs = then_expr,
      })
    else
      return self:addNode({
        tag = "while_cont",
        main_token = while_token,
        lhs = condition,
        rhs = {
          cont_expr,
          then_expr,
        }
      })
    end
  end

  self:parsePayload()
  local else_expr = self:expectStatement(false)
  return self:addNode({
    tag = "while",
    main_token = while_token,
    lhs = condition,
    rhs = {
      cont_expr,
      then_expr,
      else_expr,
    },
  })
end

function Parser:parseWhileExpr()
  local while_token = self:eatToken("keyword_while")
  if not while_token then return false end
  self:expectToken("l_paren")
  local condition = self:expectExpr()
  self:expectToken("r_paren")
  self:parsePtrPayload()
  local cont_expr = self:parseWhileContinueExpr()

  local then_expr = self:expectExpr()
  if not self:eatToken("keyword_else") then
    if not cont_expr then
      return self:addNode({
        tag = "while_simple",
        main_token = while_token,
        lhs = condition,
        rhs = then_expr,
      })
    else
      return self:addNode({
        tag = "while_cont",
        main_token = while_token,
        lhs = condition,
        rhs = {
          cont_expr,
          then_expr,
        }
      })
    end
  end
  self:parsePayload()
  local else_expr = self:expectExpr()
  return self:addNode({
    tag = "while",
    main_token = while_token,
    lhs = condition,
    rhs = {
      cont_expr,
      then_expr,
      else_expr,
    }
  })
end

function Parser:parseWhileContinueExpr()
  local colon_token = self:eatToken("colon")
  if not colon_token then
    -- TODO expected_continue_expr error
    return false
  end
  self:expectToken("l_paren")
  local node = self:parseAssignExpr()
  if not node then error("expected_expr_or_assignment") end
  self:expectToken("r_paren")
  return node
end

function Parser:parseSwitchExpr(is_labeled)
  local switch_token = self:eatToken("keyword_switch")
  if not switch_token then return false end
  return self:expectSwitchSuffix(is_labeled and (switch_token - 2) or switch_token)
end

function Parser:expectSwitchExpr(is_labeled)
  return assert(self:parseSwitchExpr(is_labeled))
end

function Parser:expectSwitchSuffix(main_token)
  self:expectToken("l_paren")
  local expr_node = self:expectExpr()
  self:expectToken("r_paren")
  self:expectToken("l_brace")
  local cases = self:parseSwitchProngList()
  local trailing_comma = self.tokens.tags[self.tok_i - 1] == "comma"
  self:expectToken("r_brace")

  return self:addNode({
    tag = trailing_comma and "switch_comma" or "switch",
    main_token = main_token,
    lhs = expr_node,
    rhs = cases,
  })
end

function Parser:parseSwitchProngList()
  local scratch_top = #self.scratch

  while true do
    local item = self:parseSwitchProng()
    if not item then break end

    table.insert(self.scratch, item)

    local tag = self:tokenTag()
    if tag == "comma" then
      self:nextToken()
    elseif
      tag == "colon" or
      tag == "r_paren" or
      tag == "r_brace" or
      tag == "r_bracket"
    then
      break
    else
      error("expected_comma_after_switch_prong")
    end
  end

  return self:sliceAndRestoreScratch(scratch_top)
end

function Parser:parseSwitchProng()
  local scratch_top = #self.scratch

  local is_inline = self:eatToken("keyword_inline")

  if not self:eatToken("keyword_else") then
    while true do
      local item = self:parseSwitchItem()
      if not item then break end
      table.insert(self.scratch, item)
      if not self:eatToken("comma") then break end
    end
    if scratch_top == #self.scratch then
      if is_inline then
        self.tok_i = self.tok_i - 1
      end
      return false
    end
  end
  local arrow_token = self:expectToken("equal_angle_bracket_right")
  self:parsePtrIndexPayload()

  local items = self:sliceAndRestoreScratch(scratch_top)
  return self:addNode({
    tag = is_inline and "switch_case_inline" or "switch_case",
    main_token = arrow_token,
    lhs = items,
    rhs = self:expectSingleAssignExpr(),
  })
end

function Parser:parseSwitchItem()
  local expr = self:parseExpr()
  if not expr then return false end

  local ellipsis_token = self:eatToken("ellipsis3")
  if ellipsis_token then
    return self:addNode({
      tag = "switch_range",
      main_token = ellipsis_token,
      lhs = expr,
      rhs = self:expectExpr(),
    })
  end

  return expr
end

local assignOpNode = {
  asterisk_equal = "assign_mul",
  slash_equal = "assign_div",
  percent_equal = "assign_mod",
  plus_equal = "assign_add",
  minus_equal = "assign_sub",
  angle_bracket_angle_bracket_left_equal = "assign_shl",
  angle_bracket_angle_bracket_left_pipe_equal = "assign_shl_sat",
  angle_bracket_angle_bracket_right_equal = "assign_shr",
  ampersand_equal = "assign_bit_and",
  caret_equal = "assign_bit_xor",
  pipe_equal = "assign_bit_or",
  asterisk_percent_equal = "assign_mul_wrap",
  plus_percent_equal = "assign_add_wrap",
  minus_percent_equal = "assign_sub_wrap",
  asterisk_pipe_equal = "assign_mul_sat",
  plus_pipe_equal = "assign_add_sat",
  minus_pipe_equal = "assign_sub_sat",
  equal = "assign",
}

function Parser:expectSingleAssignExpr()
  return assert(self:parseSingleAssignExpr(), "expected_expr_or_assignment")
end

function Parser:parseSingleAssignExpr()
  local lhs = self:parseExpr()
  if not lhs then return false end
  local tag = assignOpNode[self:tokenTag()]
  if not tag then
    return lhs
  end
  return self:addNode({
    tag = tag,
    main_token = self:nextToken(),
    lhs = lhs,
    rhs = self:expectExpr(),
  })
end

function Parser:expectAssignExpr()
  return assert(self:parseAssignExpr(), "expected_expr_or_assignment")
end

function Parser:parseAssignExpr()
  local expr = self:parseExpr()
  if not expr then return false end
  return self:finishAssignExpr(expr)
end

function Parser:finishAssignExpr(lhs)
  local tok = self:tokenTag()
  if tok == "comma" then
    return self:finishAssignDestructureExpr(lhs)
  end
  local tag = assignOpNode[tok]
  if not tag then
    return lhs
  end
  return self:addNode({
    tag = tag,
    main_token = self:nextToken(),
    lhs = lhs,
    rhs = self:expectExpr(),
  })
end

function Parser:finishAssignDestructureExpr(first_lhs)
  local scratch_top = #self.scratch

  table.insert(self.scratch, first_lhs)

  while self:eatToken("comma") do
    local expr = self:expectExpr()
    table.insert(self.scratch, expr)
  end

  local equal_token = self:expectToken("equal")

  local rhs = self:expectExpr()

  local lhs_count = #self.scratch - scratch_top
  assert(lhs_count > 1)

  local vals = self:sliceAndRestoreScratch(scratch_top)
  return self:addNode({
    tag = "assign_destructure",
    main_token = equal_token,
    lhs = vals,
    rhs = rhs,
  })
end

function Parser:parseCurlySuffixExpr()
  local lhs = self:parseTypeExpr()
  if not lhs then return false end
  local lbrace = self:eatToken("l_brace")
  if not lbrace then return lhs end

  local scratch_top = #self.scratch
  -- defer shrink scratch back to this

  local field_init = self:parseFieldInit()
  if field_init then
    table.insert(self.scratch, field_init)
    while true do
      local tok_tag = self:tokenTag()
      if tok_tag == "comma" then
        self:nextToken()
      elseif tok_tag == "r_brace" then
        self:nextToken()
        break
      elseif
        tok_tag == "colon" or
        tok_tag == "r_paren" or
        tok_tag == "r_bracket"
      then
        error("expected r_brace")
      else
        error("expected_comma_after_initializer")
      end
      if self:eatToken("r_brace") then break end
      local next_init = self:expectFieldInit()
      table.insert(self.scratch, next_init)
    end

    local comma = self.tokens.tags[self.tok_i - 2] == "comma"
    local inits = self:sliceAndRestoreScratch(scratch_top)
    assert(#inits > 0)
    return self:addNode({
      tag = comma and "struct_init_comma" or "struct_init",
      main_token = lbrace,
      lhs = lhs,
      rhs = inits,
    })
  end

  while true do
    if self:eatToken("r_brace") then break end
    local elem_init = self:expectExpr()
    table.insert(self.scratch, elem_init)

    local token_tag = self.tokens.tags[self.tok_i]
    if token_tag == "comma" then
      self:nextToken()
    elseif token_tag == "r_brace" then
      self:nextToken()
      break
    elseif
      token_tag == "colon" or
      token_tag == "r_paren" or
      token_tag == "r_bracket"
    then
      error("expected r_brace")
    else
      error("expected_comma_after_initializer")
    end
  end

  local comma = self.tokens.tags[self.tok_i - 2] == "comma"
  local inits = self:sliceAndRestoreScratch(scratch_top)
  local tag = "struct_init"
  if #inits > 0 then
    tag = comma and "array_init_comma" or "array_init"
  end
  return self:addNode({
    tag = tag,
    main_token = lbrace,
    lhs = lhs,
    rhs = inits,
  })
end

function Parser:parseFieldInit()
  if
    self.tokens.tags[self.tok_i + 0] == "period" and
    self.tokens.tags[self.tok_i + 1] == "identifier" and
    self.tokens.tags[self.tok_i + 2] == "equal"
  then
    self.tok_i = self.tok_i + 3
    return self:expectExpr()
  else
    return false
  end
end

function Parser:expectToken(tag)
  assert(self.tokens.tags[self.tok_i] == tag, "expected_token: "..tag..", found: "..self.tokens.tags[self.tok_i])
  return self:nextToken()
end

function Parser:expectSemicolon(error_tag)
  assert(self:tokenTag() == "semicolon", error_tag..", found "..self:tokenTag())
  self:nextToken()
end

function Parser:eatToken(tag)
  if self.tokens.tags[self.tok_i] == tag then
    return self:nextToken()
  end
  return false
end

function Parser:eatDocComments()
  local first_doc_comment = self:eatToken("doc_comment")
  if first_doc_comment then
    -- TODO same line doc comment error
    while self:eatToken("doc_comment") do end
    return first_doc_comment
  end
  return false
end

function Parser:setNode(i, node)
  self.nodes[i] = node
end

function Parser:addNode(node)
  table.insert(self.nodes, node)
  return #self.nodes
end

-- source must be NUL-terminated
local function parse(source)
  local tokenizer = Tokenizer.init(source)
  local tokens = {
    tags = {},
    starts = {},
    ends = {},
  }
  local token = {}
  while true do
    tokenizer:next(token)
    table.insert(tokens.tags, token.tag)
    table.insert(tokens.starts, token.start)
    table.insert(tokens.ends, token["end"])
    if token.tag == "eof" then break end
  end

  local parser = Parser.init(tokens, source)
  parser:parseRoot()

  return Ast.init(source, tokens, parser.nodes)
end

return {
  parse = parse,
  Parser = Parser,
  undefined = undefined,
}