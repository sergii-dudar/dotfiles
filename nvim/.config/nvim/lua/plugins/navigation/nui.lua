-- nui.nvim held one commit below MunifTanjim/nui.nvim@3d425a7
-- ("perf(tree): optimize redraw for large amount of nodes", 2026-08-15).
--
-- Why: that commit changed `NuiTree:set_nodes()` to drop a parent's old children via the
-- recursive `remove_node()` instead of the previous flat `by_id[node_id] = nil`. Two gaps
-- combine into a crash:
--   1. `remove_node()` indexes the looked-up node with no nil guard
--      (nui/tree/init.lua:494 `if node:has_children()`), unlike `_link()` right below it,
--      which explicitly tolerates ids missing from `by_id`.
--   2. `initialize_nodes()` early-returns for a *recycled* live node — one already
--      initialized once, so its `__children` is nil — leaving `_child_ids` pointing at
--      descendants that step 1 just deleted from `by_id`.
-- So the first `set_nodes()` leaves dangling child ids, and the next `set_nodes()` on that
-- parent recurses into one and dies with `attempt to index local 'node' (a nil value)`.
--
-- neo-tree's `group_empty_dirs` produces exactly that call shape: the "lazy load of a
-- single sub folder" branch in neo-tree/ui/renderer.lua hands *live* sibling nodes
-- straight back into `set_nodes()`. Any chain of >=2 single-child directories triggers it —
-- Java package chains (src/main/java/ua -> ua/gov -> ua/gov/bank) and the stow layout in
-- this repo (<app>/.config/<app>) alike. Before 3d425a7 the flat nil-out never
-- dereferenced a node, so the dangling ids were harmless.
--
-- Verified against a standalone NuiTree repro: pinned commit passes, current HEAD raises
-- the exact traceback, and adding a nil guard to `remove_node()` also fixes it.
-- Cost of the pin: 4 later non-tree commits (popup/layout invalid-winid guards,
-- popup size-only update_layout, nui.table features). Only neo-tree uses nui.tree here.
--
-- Remove this pin once upstream guards `remove_node()` against a missing node id.
-- last commit it's 10fc361835c856ba4233ef5ea135b919bf3dce97, where we have this problem
return {
    "MunifTanjim/nui.nvim",
    commit = "5959922693a785a0e1624b45c968c844822f1ca1",
    pin = true,
}