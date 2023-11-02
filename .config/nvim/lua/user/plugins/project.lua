return {
  "ahmedkhalf/project.nvim",
  dependencies = "nvim-telescope/telescope.nvim",
  event = "VeryLazy",
  config = function()
    require("project_nvim").setup()

    local telescope = require("telescope")
    telescope.load_extension("projects")
    vim.keymap.set("n", "<leader>tp", function() telescope.extensions.projects.projects() end, { desc = "[P]rojects" })
  end,
}
