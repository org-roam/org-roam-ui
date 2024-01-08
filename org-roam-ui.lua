local org_roam_ui = {
  prefix = "org-roam-ui-",
  link = "https://github.com/org-roam/org-roam-ui",
}

-- Root directory of the org-roam-ui project.
local org_roam_ui_root_dir = vim.fn.getcwd()

-- Directory containing org-roam-ui's web build.
local org_roam_ui_app_build_dir = org_roam_ui_root_dir .. "/out/"

local org_roam_ui_port = 35901

-- ignore theme for now
-- since defcustom is user-config
-- so I can use default value for now I guess
-- if I dont want to customize anything
-- now goes to 91th line
