-- Reference-style markdown images for snacks.image (used by queries/markdown_inline/images.scm).
--
-- `![alt][label]`, `![label][]`, `![label]` -- and plain links `[label]` whose `[label]: url` definition points to an
-- image file, which is how jdtls puts diagrams into javadoc hovers (`[1]` + `[1]: file:///.../switchIfEmpty.svg`).
-- The definitions live in the block-level markdown tree, which an inline query cannot see, so two query helpers look
-- them up in the buffer the query runs on (works for unnamed hover floats too):
--   #md-ref-dest! @capture        directive: replace the capture's text (a label) with its definition's destination;
--                                 left as is when there is no definition (snacks then tries the label as a path)
--   #md-ref-is-image? @capture    predicate: the label has a definition whose destination is an image file
-- Labels match as CommonMark says (case-insensitive, inner whitespace collapsed, first definition wins); file:// URLs
-- become paths, anything else is left to snacks' normal resolution (relative paths, img_dirs, http).
--
-- SVG: ImageMagick 7.1.2 (Homebrew) delegates SVG to rsvg-convert with an extensionless temp file and then fails with
-- "no decode delegate", so snacks cannot show any SVG. A local SVG destination is therefore rasterized here with
-- rsvg-convert into snacks' cache (once per file + mtime) on a white background: the reactor/javadoc marble diagrams
-- are black lines on transparent, invisible on a dark theme. Without rsvg-convert the SVG path is passed on as is.
local M = {}

local IMAGE_EXT = { svg = true, svgz = true }

---@param label string
---@return string
local function normalize(label)
    return (vim.trim(label):gsub("%s+", " "):lower())
end

local def_query ---@type vim.treesitter.Query?
local cache = {} ---@type table<integer, {tick: integer, defs: table<string, string>}>

---@param buf integer
---@return table<string, string> label -> destination
local function definitions(buf)
    local tick = vim.api.nvim_buf_get_changedtick(buf)
    local hit = cache[buf]
    if hit and hit.tick == tick then
        return hit.defs
    end
    local defs = {}
    local ok, parser = pcall(vim.treesitter.get_parser, buf, "markdown")
    local tree = ok and parser and parser:parse()[1]
    if tree then
        def_query = def_query
            or vim.treesitter.query.parse(
                "markdown",
                "(link_reference_definition (link_label) @label (link_destination) @dest)"
            )
        for _, match in def_query:iter_matches(tree:root(), buf, 0, -1) do
            local label, dest
            for id, nodes in pairs(match) do
                local node = type(nodes) == "table" and nodes[1] or nodes
                local text = vim.treesitter.get_node_text(node, buf)
                if def_query.captures[id] == "label" then
                    label = normalize(text:gsub("^%[", ""):gsub("%]$", ""))
                else
                    dest = text:gsub("^<", ""):gsub(">$", "")
                    if dest:find("^file://") then
                        dest = vim.uri_to_fname(dest)
                    end
                end
            end
            if label and dest and defs[label] == nil then
                defs[label] = dest
            end
        end
    end
    cache[buf] = { tick = tick, defs = defs }
    return defs
end

vim.api.nvim_create_autocmd("BufWipeout", {
    group = vim.api.nvim_create_augroup("snacks.image.refs", { clear = true }),
    callback = function(ev)
        cache[ev.buf] = nil
    end,
})

---@param node TSNode
---@param buf integer
---@return string
local function label_of(node, buf)
    return (vim.treesitter.get_node_text(node, buf):gsub("^%[", ""):gsub("%]$", ""))
end

---@param match table<integer, TSNode[]|TSNode>
---@param id integer
---@return TSNode?
local function node_of(match, id)
    local nodes = match[id]
    return type(nodes) == "table" and nodes[1] or nodes
end

---@param dest string
---@return boolean
local function is_image(dest)
    local ext = dest:gsub("[?#].*$", ""):match("%.(%w+)$")
    ext = ext and ext:lower()
    if not ext then
        return false
    end
    if IMAGE_EXT[ext] then
        return true
    end
    local formats = Snacks.image.config.formats or {}
    return vim.tbl_contains(formats, ext)
end

---@param dest string
---@return string png path, or dest unchanged
local function rasterize_svg(dest)
    if not dest:lower():find("%.svgz?$") or dest:find("^%w%w+://") or vim.fn.executable("rsvg-convert") == 0 then
        return dest
    end
    local stat = vim.uv.fs_stat(dest)
    if not stat then
        return dest
    end
    local dir = Snacks.image.config.cache .. "/svg"
    local out = ("%s/%s.png"):format(dir, vim.fn.sha256(dest .. ":" .. stat.mtime.sec):sub(1, 16))
    if vim.uv.fs_stat(out) then
        return out
    end
    vim.fn.mkdir(dir, "p")
    local res = vim.system({ "rsvg-convert", "-f", "png", "-z", "2", "-b", "white", "-o", out, dest }):wait(5000)
    return (res.code == 0 and vim.uv.fs_stat(out)) and out or dest
end

function M.setup()
    vim.treesitter.query.add_directive("md-ref-dest!", function(match, _, source, pred, metadata)
        local id = pred[2]
        local node = node_of(match, id)
        if not node or type(source) ~= "number" then
            return
        end
        local dest = definitions(source)[normalize(label_of(node, source))]
        if dest then
            metadata[id] = metadata[id] or {}
            metadata[id].text = rasterize_svg(dest)
        end
    end, { force = true, all = true })

    vim.treesitter.query.add_predicate("md-ref-is-image?", function(match, _, source, pred)
        local node = node_of(match, pred[2])
        if not node or type(source) ~= "number" then
            return false
        end
        local dest = definitions(source)[normalize(label_of(node, source))]
        return dest ~= nil and is_image(dest)
    end, { force = true, all = true })
end

return M