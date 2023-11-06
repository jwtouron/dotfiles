local function desc(s)
  return "Harpoon: "..s
end

local function mark(f)
  return "<cmd>:lua require('harpoon.mark')." .. f .. "<cr>"
end

local function ui(f)
  return "<cmd>:lua require('harpoon.ui')." .. f .. "<cr>"
end

return {
  "ThePrimeagen/harpoon",
  dependencies = 'nvim-lua/plenary.nvim',
  event = "VeryLazy",
  keys = {
    { "<M-a>", mark('add_file()'),        desc = desc("Add File") },
    { "<M-m>", ui('toggle_quick_menu()'), desc = desc("Quick Menu") },
    { "<M-]>", ui('nav_next()'),          desc = desc("Nav Next") },
    { "<M-[>", ui('nav_prev()'),          desc = desc("Nav Prev") },
    { "<M-1>", ui('nav_file(1)'),         desc = desc("Nav File 1") },
    { "<M-2>", ui('nav_file(2)'),         desc = desc("Nav File 2") },
    { "<M-3>", ui('nav_file(3)'),         desc = desc("Nav File 3") },
    { "<M-4>", ui('nav_file(4)'),         desc = desc("Nav File 4") },
    { "<M-5>", ui('nav_file(5)'),         desc = desc("Nav File 5") },
    { "<M-6>", ui('nav_file(6)'),         desc = desc("Nav File 1") },
  },
  opts = {
    tabline = false,
    -- tabline_prefix = "▏",
    -- tabline_suffix = "▕",
    tabline_prefix = "▎",
    tabline_suffix = "🮇",
    -- tabline_prefix = "▌",
    -- tabline_suffix = "▐",
  },
}
