local function mini(name, spec)
  local result = {
    'echasnovski/mini.'..name,
    version = false,
    event = "VeryLazy",
  }
  for k, v in pairs(spec or { config = true }) do
    result[k] = v
  end
  return result
end

return {
  mini("align"),
  mini("splitjoin"),
  mini("trailspace"),
}
