local augroup = vim.api.nvim_create_augroup(debug.getinfo(1, "S").source, {})

vim.api.nvim_create_autocmd("ColorScheme", {
  group = augroup,
  callback = function()
    vim.schedule(function()
      vim.cmd "highlight CursorLine gui=underline guibg=NONE"
      vim.cmd "highlight MatchParen gui=underline guibg=NONE"
    end)
  end,
})

local by_path = {}
local loaded = {}

vim.api.nvim_create_autocmd('ColorSchemePre', {
  group = augroup,
  callback = function(args)
    local scheme = args.match

    if loaded[scheme] == true then return end

    if #vim.api.nvim_get_runtime_file(string.format("colors/%s.{vim,lua}", scheme), false) > 0 then
      loaded[scheme] = true
      return
    end

    if loaded[scheme] == false then
      vim.notify("Previous attempt to find colorscheme failed", vim.log.levels.ERROR)
      return
    end

    for path, pkg in pairs(by_path) do
      if vim.uv.fs_stat(string.format("%s/colors/%s.lua", path, scheme))
        or vim.uv.fs_stat(string.format("%s/colors/%s.vim", path, scheme))
      then
        vim.cmd.packadd(pkg)
        loaded[scheme] = true
        return
      end
    end

    loaded[scheme] = false
    vim.notify("Attempt to find colorscheme failed", vim.log.levels.ERROR)
  end,
})

local function colorscheme(name, config)
  if config then config() end
  return { src = "https://github.com/" .. name }
end

local lake_dweller_config = function()
  vim.api.nvim_create_autocmd("ColorSchemePre", {
    group = augroup,
    pattern = "lake-dweller",
    once = true,
    callback = function() require("lake-dweller").setup() end
  })
end

local zen_config = function()
vim.api.nvim_create_autocmd("ColorScheme", {
  group = augroup,
  command =  "highlight MsgArea guifg=#e0e0e0",
})
end

vim.pack.add({
  colorscheme("Aejkatappaja/sora"),
  colorscheme("aktersnurra/no-clown-fiesta.nvim"),
  colorscheme("AlexvZyl/nordic.nvim"),
  colorscheme("antonk52/lake.nvim"),
  colorscheme("aymenhafeez/doric-themes.nvim"),
  colorscheme("blazkowolf/gruber-darker.nvim"),
  colorscheme("darkvoid-theme/darkvoid.nvim"),
  colorscheme("dgox16/oldworld.nvim"),
  colorscheme("hardselius/warlock"),
  colorscheme("jnurmine/Zenburn"),
  colorscheme("kdheepak/monochrome.nvim"),
  colorscheme("kvrohit/rasmus.nvim"),
  colorscheme("mcauley-penney/techbase.nvim"),
  colorscheme("mellow-theme/mellow.nvim"),
  colorscheme("miikanissi/modus-themes.nvim"),
  colorscheme("nendix/zen.nvim", zen_config),
  colorscheme("oskarnurm/koda.nvim"),
  colorscheme("p00f/alabaster.nvim"),
  colorscheme("rafamadriz/neon"),
  colorscheme("ramojus/mellifluous.nvim"),
  colorscheme("rebelot/kanagawa.nvim"),
  colorscheme("rjshkhr/shadow.nvim"),
  colorscheme("rose-pine/neovim"),
  colorscheme("sainnhe/gruvbox-material", function() vim.g.gruvbox_material_better_performance = 1 end),
  colorscheme("scottmckendry/cyberdream.nvim"),
  colorscheme("shaunsingh/nord.nvim"),
  colorscheme("webhooked/kanso.nvim"),
}, { confirm = false, load = function(plugin) by_path[plugin.path] = plugin.spec.name end})
