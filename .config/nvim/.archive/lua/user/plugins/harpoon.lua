local function desc(s)
  return "Harpoon: "..s
end

return {
  'ThePrimeagen/harpoon',
  branch = "harpoon2",
  dependencies = 'nvim-lua/plenary.nvim',
  keys = function()
    local harpoon = require("harpoon")
    local keys = {
      { "<A-a>", function() harpoon:list():add() end,                          desc = desc("Add File") },
      { "<A-m>", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end,  desc = desc("Toggle Quick Menu") },
      { "<A-l>", function() harpoon:list():next({ ui_nav_wrap = true }) end,   desc = desc("Nav Next") },
      { "<A-h>", function() harpoon:list():prev({ ui_nav_wrap = true }) end,   desc = desc("Nav Prev") },
      { "<A-1>", function() harpoon:list():select(1) end,                      desc = desc("Nav File 1") },
      { "<A-2>", function() harpoon:list():select(2) end,                      desc = desc("Nav File 2") },
      { "<A-3>", function() harpoon:list():select(3) end,                      desc = desc("Nav File 3") },
      { "<A-4>", function() harpoon:list():select(4) end,                      desc = desc("Nav File 4") },
      { "<A-5>", function() harpoon:list():select(5) end,                      desc = desc("Nav File 5") },
      { "<A-6>", function() harpoon:list():select(6) end,                      desc = desc("Nav File 6") },
      { "<Tab>", function() harpoon:list():next({ ui_nav_wrap = true }) end,   desc = desc("Nav Next") },
      { "<S-Tab>", function() harpoon:list():prev({ ui_nav_wrap = true }) end, desc = desc("Nav Prev") },
    }
    return keys
  end,
  opts = {
    settings = {
      save_on_toggle = true,
    }
  },
}
