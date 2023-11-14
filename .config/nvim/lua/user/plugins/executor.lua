return {
  "google/executor.nvim",
  enabled = false,
  dependencies = "MunifTanjim/nui.nvim",
  cmd = {
    "ExecutorRun",
    "ExecutorSetCommand"
  },
  keys = {
    { "<leader>ec", "<cmd>ExecutorSetCommand<cr>",   desc = "Set [C]ommand" },
    { "<leader>ed", "<cmd>ExecutorToggleDetail<cr>", desc = "Toggle [D]etail" },
    { "<leader>eh", "<cmd>ExecutorShowHistory<cr>",  desc = "Show [H]istory" },
    { "<leader>er", "<cmd>ExecutorRun<cr>",          desc = "[R]un" },
    { "<leader>eR", "<cmd>ExecutorReset<cr>",        desc = "[R]eset" },
  },
  opts = {
    use_split = false,
  },
}
