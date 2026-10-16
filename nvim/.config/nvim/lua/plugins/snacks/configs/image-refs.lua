-- snacks.image `resolve` hook: reference-style markdown images.
--
-- queries/markdown_inline/images.scm turns `![alt][label]`, `![label][]` and `![label]` into the src "mdref:<label>".
-- Here the label is matched against the `[label]: url "title"` definitions of the markdown buffer (CommonMark: labels
-- are case-insensitive, inner whitespace collapses), and the destination goes through snacks' normal resolution
-- (relative to cwd / the file, img_dirs, http urls). Any other src returns nil, i.e. snacks' default handling.
local M = {}

local PREFIX = "mdref:"

---@param label string
---@return string
local function normalize(label)
    return vim.trim(label):gsub("%s+", " "):lower()
end

---@param buf integer
---@return table<string, string> label -> destination
local function definitions(buf)
    local defs = {}
    local ok, parser = pcall(vim.treesitter.get_parser, buf, "markdown")
    if not ok or not parser then
        return defs
    end
    local tree = parser:parse()[1]
    if not tree then
        return defs
    end
    local query = vim.treesitter.query.parse(
        "markdown",
        "(link_reference_definition (link_label) @label (link_destination) @dest)"
    )
    for _, match in query:iter_matches(tree:root(), buf, 0, -1) do
        local label, dest
        for id, nodes in pairs(match) do
            local node = type(nodes) == "table" and nodes[1] or nodes
            local text = vim.treesitter.get_node_text(node, buf)
            if query.captures[id] == "label" then
                label = normalize(text:gsub("^%[", ""):gsub("%]$", ""))
            else
                dest = text:gsub("^<", ""):gsub(">$", "")
            end
        end
        -- CommonMark: the first definition of a label wins
        if label and dest and defs[label] == nil then
            defs[label] = dest
        end
    end
    return defs
end

---@param file string
---@param src string
---@return string?
function M.resolve(file, src)
    if src:sub(1, #PREFIX) ~= PREFIX then
        return nil
    end
    local label = src:sub(#PREFIX + 1)
    local buf = vim.fn.bufnr(file)
    if buf == -1 then
        return nil
    end
    local dest = definitions(buf)[normalize(label)] or label
    -- dest never starts with the prefix, so this lands in snacks' default resolution, not back here
    return Snacks.image.doc.resolve(buf, dest)
end

return M