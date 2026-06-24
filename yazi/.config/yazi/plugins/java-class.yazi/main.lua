--- Preview compiled Java `.class` files by decompiling them with Fernflower,
--- mirroring the Neovim/Snacks `.class` previewer (scripts/java/decompile_stdout.sh:
--- Fernflower -> bat, ANSI on stdout).
---
--- Why preload + cache: yazi re-invokes `peek` on every scroll step. Running the
--- decompiler (a JVM spawn, ~0.5-1s) per scroll would be unusable, so `preload`
--- decompiles ONCE into a per-file cache and `peek` only reads/scrolls that cache.
--- The cache key (ya.file_cache) hashes the file's content/metadata, so a
--- recompiled .class is re-decompiled automatically — fresh on change, cheap to
--- scroll.
local M = {}

local SCRIPT = os.getenv("HOME") .. "/dotfiles/scripts/java/decompile_stdout.sh"

--- Stable cache url for a file. `skip` is pinned to 0 so scrolling (which changes
--- job.skip) doesn't move the cache target — we decompile the whole class once.
local function cache_of(job)
	return ya.file_cache { file = job.file, skip = 0 }
end

--- Decompile into `cache` if it isn't there yet. Returns true on success/already
--- cached, false if decompilation produced nothing.
local function ensure_cache(job, cache)
	if not cache then
		return false
	end
	if fs.cha(cache) then
		return true
	end

	local output = Command(SCRIPT):arg({ tostring(job.file.url) }):output()
	if not output or output.stdout == "" then
		return false
	end

	fs.write(cache, output.stdout)
	return true
end

function M:preload(job)
	ensure_cache(job, cache_of(job))
	return true
end

function M:peek(job)
	local cache = cache_of(job)
	if not ensure_cache(job, cache) then
		return require("empty").msg(job, "Fernflower decompilation failed")
	end

	-- Read only the visible window [skip, skip+area.h) from the cached ANSI.
	local child =
		Command("cat"):arg({ tostring(cache) }):stdout(Command.PIPED):stderr(Command.PIPED):spawn()
	if not child then
		return require("empty").msg(job, "Failed to read decompiled cache")
	end

	local limit = job.area.h
	local i, lines = 0, ""
	repeat
		local line, event = child:read_line()
		if event ~= 0 then
			break
		end
		i = i + 1
		if i > job.skip then
			lines = lines .. line
		end
	until i >= job.skip + limit
	child:start_kill()

	if job.skip > 0 and i < job.skip + limit then
		-- Scrolled past EOF: clamp back so the last page stays in view.
		ya.emit("peek", { math.max(0, i - limit), only_if = job.file.url, upper_bound = true })
	else
		lines = lines:gsub("\t", string.rep(" ", rt.preview.tab_size))
		ya.preview_widget(job, ui.Text.parse(lines):area(job.area))
	end
end

function M:seek(job)
	local h = cx.active.current.hovered
	if not h or h.url ~= job.file.url then
		return
	end

	local step = math.floor(job.units * job.area.h / 10)
	step = step == 0 and ya.clamp(-1, job.units, 1) or step
	ya.emit("peek", { math.max(0, cx.active.preview.skip + step), only_if = job.file.url })
end

return M
