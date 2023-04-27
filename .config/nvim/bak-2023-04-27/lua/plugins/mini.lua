local function mini(name, opts)
  return {
    'echasnovski/mini.'..name,
    version = false,
    config = function() require('mini.'..name).setup(opts) end
  }
end

return {
  mini('ai'),
  mini('bracketed'),
  mini('bufremove'),
  mini('comment'),
  mini('jump'),
  mini('move'),
  mini('surround'),
  mini('trailspace'),
}
