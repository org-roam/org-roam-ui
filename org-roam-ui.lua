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
-- and ignore defcustom since it's user-config
-- so I can use default value for now I guess
-- if I dont want to customize anything
-- now goes to 128th line

-- Internal vars
--
-- Var to keep track of which node you are looking at.
local org_roam_ui__ws_current_node = nil
-- The websocket for org-roam-ui.
local org_roam_ui_ws_socket = nil
-- The window for displaying nodes opened from within ORUI.
-- This is mostly to prevent issues with EXWM and the Webkit browser.
local org_roam_ui__window = nil
-- The websocket server for org-roam-ui.
local org_roam_ui_ws_server = nil
