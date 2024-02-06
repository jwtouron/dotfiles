local function colorscheme(name, spec)
  local ret = {
    name,
    lazy = true,
  }
  for k, v in pairs(spec or {}) do
    ret[k] = v
  end
  return ret
end

return {
  colorscheme("Badacadabra/vim-archery"),
  colorscheme("mcchrish/zenbones.nvim", { init = function() vim.g.bones_compat = true end }),
  colorscheme("navarasu/onedark.nvim"),
  colorscheme("ramojus/mellifluous.nvim"),
  colorscheme("rebelot/kanagawa.nvim"),
  colorscheme("romainl/Apprentice"),
  colorscheme("rose-pine/neovim"),
  -- Simple Colorschemes
  colorscheme("fxn/vim-monochrome"),
  colorscheme("hardselius/warlock"),
  colorscheme("widatama/vim-phoenix"),
}
