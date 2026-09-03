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
--- Plain wheel/trackpad scroll pans (most mice, including vertical-scroll-only
--- ones, send this); ctrl+wheel/trackpad-pinch/ctrl+=/ctrl+- zoom instead, so
--- one input isn't overloaded for both. Arrow keys pan in all four directions
--- regardless of what the pointing device can do.
---
--- It also hides the viewer's chrome (status pill, filename, server URL,
--- timestamp) and the framing padding/outline, so the diagram gets the whole
--- viewport. `h` toggles the chrome back on. The one signal the header carried
--- that matters — a dead connection, meaning the diagram on screen is stale —
--- is surfaced as a floating pill instead.
---
--- Nothing under `~/.local/share/nvim/lazy/plantuml.nvim/` is modified: the
--- injection hooks `plantuml.server.start`, wrapping the `get_html` callback it
--- receives, so plugin updates cannot clobber it.

local M = {}

--- Injected before `</body>`, so it runs after the viewer's own inline script
--- and can neutralise it.
local VIEWER_PATCH = [==[
<style>
  /* Full-bleed: the header (live/updated status, filename, server URL) and the
     framing padding + outline are overhead for a diagram viewer. Toggle with `h`. */
  body.pv-chrome-hidden .top {
    display: none;
  }

  body.pv-chrome-hidden .wrap {
    padding: 0;
  }

  body.pv-chrome-hidden .board {
    border-radius: 0;
    outline: none;
  }

  /* We manage the image's box ourselves; the viewer's fit-to-board caps would
     otherwise clamp the zoomed size. */
  #img {
    max-width: none !important;
    max-height: none !important;
    cursor: inherit !important;
  }

  /* Shown only while the connection is down, since the hidden header can no
     longer warn that the diagram is stale. */
  #pv-status {
    position: absolute;
    left: .5rem;
    top: .5rem;
    padding: .15rem .5rem;
    border-radius: 999px;
    background: var(--err);
    color: #fff;
    font-size: .75rem;
    font-weight: 500;
    pointer-events: none;
    display: none;
  }

  #pv-status.visible {
    display: block;
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

    // Give the diagram the whole viewport; `h` brings the header back.
    document.body.classList.add("pv-chrome-hidden");

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

    // The header is hidden, so re-surface the only state it carried that the
    // diagram itself cannot show: the feed is down, so this render is stale.
    // Transient "Loading.../Starting..." (warn) states are deliberately ignored.
    var statusPill = document.createElement("div");
    statusPill.id = "pv-status";
    board.appendChild(statusPill);

    function reflectStatus(kind, text) {
      if (kind === "err") {
        statusPill.textContent = text || "disconnected";
        statusPill.classList.add("visible");
      } else {
        statusPill.classList.remove("visible");
      }
    }

    if (typeof window.setStatus === "function") {
      var originalSetStatus = window.setStatus;
      window.setStatus = function (kind, text) {
        originalSetStatus(kind, text);
        reflectStatus(kind, text);
      };
    }

    function natW() { return img.naturalWidth || 0; }
    function natH() { return img.naturalHeight || 0; }

    // Deliberately uncapped at 1x, unlike the viewer's own fit: an SVG scaled up
    // stays sharp, so a small diagram should fill the page rather than sit in it.
    function fitScale() {
      var r = board.getBoundingClientRect();
      if (!natW() || !natH()) return 1;
      var s = Math.min(r.width / natW(), r.height / natH());
      return Math.max(MIN_SCALE, Math.min(MAX_SCALE, s));
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

    // Fit the width and anchor to the top. For a tall sequence diagram this uses
    // the full page width that fit-to-board would otherwise leave letterboxed.
    function fitToWidth() {
      var r = board.getBoundingClientRect();
      if (!natW() || !natH()) return;
      scale = Math.max(MIN_SCALE, Math.min(MAX_SCALE, r.width / natW()));
      panX = 0;
      panY = Math.max(0, (natH() * scale - r.height) / 2);
      apply(true);
    }

    // deltaMode 1 ("lines") reports small integers instead of pixels; scale it
    // up so plain-scroll panning doesn't crawl on mice/browsers that use it.
    function wheelDelta(raw, mode) {
      return mode === 1 ? raw * 20 : raw;
    }

    board.addEventListener("wheel", function (e) {
      if (!natW()) return;
      e.preventDefault(); // also stops the page from trying to scroll/pinch-zoom itself
      var r = board.getBoundingClientRect();
      // Trackpad pinch-zoom is reported as wheel+ctrlKey by the browser, same as
      // holding ctrl over a real wheel — both zoom. Plain wheel/two-finger
      // scroll (no ctrl) pans instead, since that's what most mice send.
      if (e.ctrlKey || e.metaKey) {
        var factor = e.deltaY < 0 ? STEP : 1 / STEP;
        zoomAt(effective() * factor, e.clientX - r.left, e.clientY - r.top);
        return;
      }
      panX -= wheelDelta(e.deltaX, e.deltaMode);
      panY -= wheelDelta(e.deltaY, e.deltaMode);
      apply(false);
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

    var ARROW_PAN_STEP = 80;

    window.addEventListener("keydown", function (e) {
      var r = board.getBoundingClientRect();
      var cx = r.width / 2;
      var cy = r.height / 2;

      // Ctrl+=/Ctrl+- drive our zoom instead of the browser's own page zoom.
      if (e.ctrlKey && !e.metaKey && !e.altKey && (e.key === "+" || e.key === "=" || e.key === "-" || e.key === "_")) {
        zoomAt(effective() * (e.key === "-" || e.key === "_" ? 1 / STEP : STEP), cx, cy);
        e.preventDefault();
        return;
      }

      if (e.metaKey || e.ctrlKey || e.altKey) return; // leave every other modified key to the browser

      if (e.key === "+" || e.key === "=") {
        zoomAt(effective() * STEP, cx, cy);
      } else if (e.key === "-" || e.key === "_") {
        zoomAt(effective() / STEP, cx, cy);
      } else if (e.key === "0") {
        resetToFit();
      } else if (e.key === "1") {
        zoomAt(1, cx, cy);
      } else if (e.key === "w" || e.key === "W") {
        fitToWidth();
      } else if (e.key === "h" || e.key === "H") {
        document.body.classList.toggle("pv-chrome-hidden");
        apply(false);
      } else if (e.key === "ArrowUp") {
        panY += ARROW_PAN_STEP;
        apply(false);
      } else if (e.key === "ArrowDown") {
        panY -= ARROW_PAN_STEP;
        apply(false);
      } else if (e.key === "ArrowLeft") {
        panX += ARROW_PAN_STEP;
        apply(false);
      } else if (e.key === "ArrowRight") {
        panX -= ARROW_PAN_STEP;
        apply(false);
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
