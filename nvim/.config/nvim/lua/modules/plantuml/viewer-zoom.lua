--- Continuous, crisp zoom for the plantuml.nvim browser viewer.
---
--- The vendored viewer (`plantuml.nvim/lua/plantuml/assets/viewer.html`) ships a
--- binary zoom: the diagram is either fit-to-board or at 100% natural size, and
--- `resetZoomPan()` is invoked on every live update, so the view snaps back to
--- fit on each save. Neither is workable for tall sequence diagrams.
---
--- This module injects a self-contained CSS+JS block into the HTML the plugin
--- serves, replacing that layer with continuous cursor-centred zoom that is
--- preserved across live updates. Sizing is done by layout (`width`/`height` in
--- px) rather than `transform: scale()`, so the SVG re-rasterises crisply at
--- every zoom level.
---
--- Nothing under `~/.local/share/nvim/lazy/plantuml.nvim/` is modified: the
--- injection hooks `plantuml.server.start`, wrapping the `get_html` callback it
--- receives, so plugin updates cannot clobber it.

local M = {}

--- Injected before `</body>`, so it runs after the viewer's own inline script
--- and can neutralise it.
local VIEWER_PATCH = [==[
<style>
  /* We manage the image's box ourselves; the viewer's fit-to-board caps would
     otherwise clamp the zoomed size. */
  #img {
    max-width: none !important;
    max-height: none !important;
    cursor: inherit !important;
  }

  #zoom-badge {
    position: absolute;
    right: .5rem;
    bottom: .5rem;
    padding: .15rem .45rem;
    border-radius: 999px;
    background: var(--pill-bg);
    color: var(--muted);
    font-size: .75rem;
    font-weight: 500;
    opacity: 0;
    transition: opacity .2s ease-in-out;
    pointer-events: none;
  }

  #zoom-badge.visible {
    opacity: 1;
  }
</style>
<script>
  (function () {
    "use strict";

    var board = document.getElementById("board");
    var img = document.getElementById("img");
    if (!board || !img) return;

    // --- Neutralise the viewer's own binary fit/natural-size zoom-pan layer ---
    // Its handlers are top-level function declarations, so they hang off window.
    if (typeof window.handleBoardMouseDown === "function") {
      board.removeEventListener("mousedown", window.handleBoardMouseDown);
    }
    if (typeof window.handleMouseMove === "function") {
      document.removeEventListener("mousemove", window.handleMouseMove);
    }
    if (typeof window.handleMouseUp === "function") {
      document.removeEventListener("mouseup", window.handleMouseUp);
    }
    // The viewer calls resetZoomPan() on every live update; keeping the view
    // across updates is the whole point of this patch.
    window.resetZoomPan = function () {};
    window.updateImageTransform = function () {};
    board.classList.remove("zoom-pan-mode", "dragging");

    var MIN_SCALE = 0.05;
    var MAX_SCALE = 40;
    var STEP = 1.15;
    var BADGE_LINGER = 1200;

    var scale = null; // null => track fit-to-board until the user zooms
    var panX = 0;
    var panY = 0;
    var dragging = false;
    var dragStartX = 0;
    var dragStartY = 0;
    var dragPanX = 0;
    var dragPanY = 0;

    var badge = document.createElement("div");
    badge.id = "zoom-badge";
    board.appendChild(badge);
    var badgeTimer = null;

    function natW() { return img.naturalWidth || 0; }
    function natH() { return img.naturalHeight || 0; }

    function fitScale() {
      var r = board.getBoundingClientRect();
      if (!natW() || !natH()) return 1;
      return Math.min(r.width / natW(), r.height / natH(), 1);
    }

    function effective() {
      return scale === null ? fitScale() : scale;
    }

    function clampPan(s) {
      var r = board.getBoundingClientRect();
      var maxX = Math.max(0, (natW() * s - r.width) / 2);
      var maxY = Math.max(0, (natH() * s - r.height) / 2);
      panX = Math.max(-maxX, Math.min(maxX, panX));
      panY = Math.max(-maxY, Math.min(maxY, panY));
    }

    function showBadge(s) {
      badge.textContent = Math.round(s * 100) + "%";
      badge.classList.add("visible");
      if (badgeTimer) clearTimeout(badgeTimer);
      badgeTimer = setTimeout(function () {
        badge.classList.remove("visible");
      }, BADGE_LINGER);
    }

    function apply(showLabel) {
      if (!natW() || !natH()) return;
      var s = effective();
      clampPan(s);
      // Size by layout rather than transform: scale() so the browser
      // re-rasterises the SVG at the new size instead of scaling a cached raster.
      img.style.width = natW() * s + "px";
      img.style.height = natH() * s + "px";
      img.style.transform = "translate(" + panX + "px," + panY + "px)";
      var overflows = natW() * s > board.clientWidth || natH() * s > board.clientHeight;
      board.style.cursor = overflows ? (dragging ? "grabbing" : "grab") : "default";
      if (showLabel) showBadge(s);
    }

    // Zoom so the diagram point under (cx, cy) stays put. cx/cy are board-relative.
    function zoomAt(target, cx, cy) {
      var r = board.getBoundingClientRect();
      var cur = effective();
      var w = natW() * cur;
      var h = natH() * cur;
      var fx = w ? (cx - r.width / 2 - panX) / w + 0.5 : 0.5;
      var fy = h ? (cy - r.height / 2 - panY) / h + 0.5 : 0.5;
      scale = Math.max(MIN_SCALE, Math.min(MAX_SCALE, target));
      panX = cx - r.width / 2 - (fx - 0.5) * natW() * scale;
      panY = cy - r.height / 2 - (fy - 0.5) * natH() * scale;
      apply(true);
    }

    function resetToFit() {
      scale = null;
      panX = 0;
      panY = 0;
      apply(true);
    }

    board.addEventListener("wheel", function (e) {
      if (!natW()) return;
      e.preventDefault(); // also captures trackpad pinch (ctrl+wheel)
      var r = board.getBoundingClientRect();
      var factor = e.deltaY < 0 ? STEP : 1 / STEP;
      zoomAt(effective() * factor, e.clientX - r.left, e.clientY - r.top);
    }, { passive: false });

    board.addEventListener("mousedown", function (e) {
      if (e.button !== 0 || !natW()) return;
      dragging = true;
      dragStartX = e.clientX;
      dragStartY = e.clientY;
      dragPanX = panX;
      dragPanY = panY;
      apply(false);
      e.preventDefault();
    });

    document.addEventListener("mousemove", function (e) {
      if (!dragging) return;
      panX = dragPanX + (e.clientX - dragStartX);
      panY = dragPanY + (e.clientY - dragStartY);
      apply(false);
    });

    document.addEventListener("mouseup", function () {
      if (!dragging) return;
      dragging = false;
      apply(false);
    });

    board.addEventListener("dblclick", function (e) {
      var r = board.getBoundingClientRect();
      if (scale === null) {
        zoomAt(1, e.clientX - r.left, e.clientY - r.top);
      } else {
        resetToFit();
      }
    });

    window.addEventListener("keydown", function (e) {
      if (e.metaKey || e.ctrlKey || e.altKey) return; // leave native browser zoom alone
      var r = board.getBoundingClientRect();
      var cx = r.width / 2;
      var cy = r.height / 2;
      if (e.key === "+" || e.key === "=") {
        zoomAt(effective() * STEP, cx, cy);
      } else if (e.key === "-" || e.key === "_") {
        zoomAt(effective() / STEP, cx, cy);
      } else if (e.key === "0") {
        resetToFit();
      } else if (e.key === "1") {
        zoomAt(1, cx, cy);
      } else {
        return;
      }
      e.preventDefault();
    });

    // Re-apply after each live update. Panning/zoom is deliberately preserved;
    // only an untouched (fit-tracking) view re-centres.
    img.addEventListener("load", function () {
      if (scale === null) {
        panX = 0;
        panY = 0;
      }
      apply(false);
    });

    window.addEventListener("resize", function () {
      apply(false);
    });

    if (img.complete && natW()) apply(false);
  })();
</script>
]==]

--- Inject the zoom layer just before `</body>`.
--- @param html string
--- @return string
function M.patch_html(html)
    if type(html) ~= "string" or html == "" then
        return html
    end
    if html:find("zoom-badge", 1, true) then
        return html -- already patched
    end
    local patched, count = html:gsub("</body>", VIEWER_PATCH .. "</body>", 1)
    if count == 0 then
        vim.notify("plantuml: could not inject viewer zoom patch (no </body> in viewer HTML)", vim.log.levels.WARN)
        return html
    end
    return patched
end

--- Wrap `plantuml.server.start` so the HTML it serves carries the zoom patch.
--- Idempotent.
function M.setup()
    local ok, server = pcall(require, "plantuml.server")
    if not ok then
        vim.notify("plantuml: could not load plantuml.server to patch viewer", vim.log.levels.WARN)
        return
    end
    if server.__zoom_patched then
        return
    end

    local original_start = server.start
    server.start = function(port, callbacks)
        if type(callbacks) == "table" and type(callbacks.get_html) == "function" then
            local original_get_html = callbacks.get_html
            callbacks.get_html = function(...)
                return M.patch_html(original_get_html(...))
            end
        end
        return original_start(port, callbacks)
    end
    server.__zoom_patched = true
end

return M
