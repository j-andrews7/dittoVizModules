// The source download bundles an image of each plot alongside the interactive
// HTML and the CSVs. That image has to be photographed in the browser: almost
// every module ends as a plotly graph, and a great deal is applied to it after
// the figure is built -- reference lines, statistical brackets, restyled axes
// and legends, annotations the user dragged into place. Re-drawing server-side
// would quietly lose all of it, and R has no plotly-to-SVG renderer anyway.
//
// A download handler cannot ask for the picture itself: Shiny serves a session
// on one thread, so blocking in content() to wait for a websocket message would
// deadlock the very message it waits on. So the capture happens *before* the
// download starts. The first click on the button is intercepted, the graphs are
// photographed, the results are sent to the server, and only once the server
// acknowledges is the click re-fired -- this time let through, so the browser
// makes its ordinary download request and the handler finds the images waiting.
//
// Every failure path ends in the same place: the archive downloads exactly as
// it did before this existed, minus the pictures. A plot that cannot be
// photographed is never a reason to withhold someone's data.
(function () {
    window.VizModules = window.VizModules || {};

    // One graph, one format. Plotly.toImage on a dense figure is not instant.
    var CAPTURE_TIMEOUT_MS = 10000;
    // Click to server acknowledgement. Matches the Figure Builder's own SVG
    // round trip; past this the download goes out without images.
    var ROUNDTRIP_TIMEOUT_MS = 20000;
    // Safety net for the armed flag, in case a re-fired click never lands.
    var REARM_TIMEOUT_MS = 5000;
    // A single image, and the whole payload. A scatter plot with tens of
    // thousands of points produces megabytes of markup, and a websocket frame
    // is a bad place to discover that. The server re-checks these.
    var MAX_IMAGE_CHARS = 8e6;
    var MAX_TOTAL_CHARS = 24e6;
    // Rasterise at twice the on-screen resolution so the PNG is a usable image
    // rather than a screenshot.
    var PNG_SCALE = 2;
    // Below this a graph has not been laid out -- it is on a hidden tab, or
    // collapsed to nothing -- and there is nothing to photograph.
    var MIN_CAPTURE_PX = 10;

    var requests = {};

    // Plotly hands back a data URL; the markup inside is what we want. The XML
    // prolog and doctype are dropped so the server is always handed a fragment,
    // which is also what draw_to_svg() produces (.svg_standalone() restates the
    // namespace at the point the file is written).
    function decodeSvgDataUrl(url) {
        if (!url) { return null; }
        var comma = url.indexOf(',');
        if (comma === -1) { return null; }
        var head = url.slice(0, comma);
        var data = url.slice(comma + 1);
        var svg = head.indexOf('base64') !== -1 ? atob(data) : decodeURIComponent(data);
        return svg.replace(/<\?xml[^>]*\?>/i, '').replace(/<!DOCTYPE[^>]*>/i, '').trim();
    }

    // The server wants the base64 payload on its own, to decode straight to
    // bytes.
    function stripPngDataUrl(url) {
        if (!url) { return null; }
        var marker = 'base64,';
        var at = url.indexOf(marker);
        return at === -1 ? null : url.slice(at + marker.length);
    }

    // A hung promise must not hold the download open; resolve null and move on.
    function withTimeout(promise, ms) {
        return Promise.race([
            promise,
            new Promise(function (resolve) { setTimeout(function () { resolve(null); }, ms); })
        ]);
    }

    function sizeOf(el) {
        var r = el.getBoundingClientRect();
        return { width: Math.round(r.width), height: Math.round(r.height) };
    }

    function isPlotlyGraph(el) {
        return !!(el && el.classList && el.classList.contains('js-plotly-plot'));
    }

    // A single module's scope *is* its output element, so it may be the graph
    // itself; a Figure Builder card is a wrapper, so the graph is inside it.
    function targetFromScope(scope, key) {
        var box = scope.querySelector('.viz-panel-body') || scope;
        return {
            key: key,
            gd: isPlotlyGraph(scope) ? scope : scope.querySelector('.js-plotly-plot'),
            box: box,
            size: sizeOf(box)
        };
    }

    // Find the one output belonging to a module, given its namespace prefix.
    // A module's output id is '<prefix><name>'; anything with a further '-' in
    // the remainder belongs to a module nested deeper and is somebody else's.
    function findOutput(prefix) {
        // InteractiveComplexHeatmap rewrites every non-word character in the id
        // it is handed, so 'mod-Heatmap' reaches the DOM as 'mod_Heatmap'. This
        // mirrors .heatmap_widget_id() on the R side.
        var alt = prefix.replace(/[^A-Za-z0-9]/g, '_');
        var nodes = document.querySelectorAll(
            '.js-plotly-plot, .html-widget, .shiny-plot-output');
        var fallback = null;

        for (var i = 0; i < nodes.length; i++) {
            var id = nodes[i].id || '';
            var rest = null;
            if (prefix && id.indexOf(prefix) === 0) {
                rest = id.slice(prefix.length);
            } else if (alt !== prefix && id.indexOf(alt) === 0) {
                rest = id.slice(alt.length);
            }
            if (rest === null || !rest.length || rest.indexOf('-') !== -1) { continue; }

            var target = targetFromScope(nodes[i], 'Data');
            // A plotly graph is what we can actually photograph, so it wins
            // over a static plot that happens to appear earlier in the DOM.
            if (target.gd) { return target; }
            if (!fallback) { fallback = target; }
        }
        return fallback;
    }

    // What this button is responsible for. R marks the button rather than
    // leaving the script to guess from ids.
    function targets(btn) {
        var ns = btn.getAttribute('data-viz-source-ns') || '';
        var canvasId = btn.getAttribute('data-viz-canvas');

        // The Figure Builder: one image per card, keyed by the bare panel id.
        // The archive names each entry after the panel's label, which the user
        // can edit and the browser never sees, so the id is what the two sides
        // agree on.
        if (canvasId) {
            var canvas = document.getElementById(canvasId);
            if (!canvas) { return []; }
            var cards = canvas.querySelectorAll('.viz-panel-card');
            var out = [];
            for (var i = 0; i < cards.length; i++) {
                var id = cards[i].id || '';
                if (ns && id.indexOf(ns) === 0) { id = id.slice(ns.length); }
                var pid = id.replace(/_card$/, '');
                if (pid) { out.push(targetFromScope(cards[i], pid)); }
            }
            return out;
        }

        var single = findOutput(ns);
        return single ? [single] : [];
    }

    function captureTarget(t) {
        var rec = { key: t.key, width: t.size.width, height: t.size.height };

        if (!t.gd || !window.Plotly ||
            rec.width < MIN_CAPTURE_PX || rec.height < MIN_CAPTURE_PX) {
            // Not a plotly graph. Shiny inlines a renderPlot() result as a
            // base64 PNG, so there is a picture here for the taking -- but it
            // is a screenshot at whatever resolution the on-screen device
            // happened to use, and a module that can redraw itself does far
            // better. Flagged so the server treats it as a last resort rather
            // than as the authoritative capture a Plotly.toImage() result is.
            var img = t.box.querySelector('img');
            var prefix = 'data:image/png;base64,';
            if (img && img.src && img.src.indexOf(prefix) === 0) {
                rec.png = img.src.slice(prefix.length);
                rec.png_fallback = true;
            }
            return Promise.resolve(rec);
        }

        var opts = { width: rec.width, height: rec.height };
        // Sequential, not parallel: two toImage calls against one graph div
        // collide on plotly's internal clone. One format failing must not cost
        // us the other, hence the catch between them.
        return withTimeout(
            Plotly.toImage(t.gd, { format: 'svg', width: opts.width, height: opts.height }),
            CAPTURE_TIMEOUT_MS
        ).then(function (url) {
            if (url) { rec.svg = decodeSvgDataUrl(url); }
        }).catch(function () {
            /* no SVG; still try the PNG */
        }).then(function () {
            return withTimeout(
                Plotly.toImage(t.gd, {
                    format: 'png', width: opts.width, height: opts.height, scale: PNG_SCALE
                }),
                CAPTURE_TIMEOUT_MS
            );
        }).then(function (url) {
            if (url) { rec.png = stripPngDataUrl(url); }
        }).catch(function () {
            /* no PNG either; the record still carries its size */
        }).then(function () {
            if (!rec.svg) { delete rec.svg; }
            if (!rec.png) { delete rec.png; }
            return rec;
        });
    }

    // Keep the payload to a size a websocket can carry. Anything individually
    // absurd goes first, then the rest is trimmed to a budget -- PNG before
    // SVG, because the vector file is the one worth having.
    function capPayload(records) {
        var total = 0;
        records.forEach(function (r) {
            if (r.svg && r.svg.length > MAX_IMAGE_CHARS) { delete r.svg; }
            if (r.png && r.png.length > MAX_IMAGE_CHARS) { delete r.png; }
        });
        ['png', 'svg'].forEach(function (field) {
            records.forEach(function (r) {
                var n = r[field] ? r[field].length : 0;
                if (!n) { return; }
                if (total + n > MAX_TOTAL_CHARS) {
                    if (window.console && console.warn) {
                        console.warn('VizModules: source download dropped an oversized ' +
                            field + ' for "' + r.key + '".');
                    }
                    delete r[field];
                } else {
                    total += n;
                }
            });
        });
        return records;
    }

    function captureAll(list) {
        return Promise.all(list.map(function (t) {
            try {
                return captureTarget(t);
            } catch (e) {
                // A synchronous throw would escape the promise chain entirely
                // and leave the button disabled with no download ever firing.
                // The record still carries its size, so the server can redraw.
                return Promise.resolve({
                    key: t.key, width: t.size.width, height: t.size.height
                });
            }
        })).then(capPayload);
    }

    // R boxes a scalar into a length-1 array on the way out.
    function unbox(x) { return Array.isArray(x) ? x[0] : x; }

    function request(btn, images) {
        return new Promise(function (resolve) {
            var nonce = String(Date.now()) + '-' + Math.random().toString(36).slice(2);
            var settled = false;
            function finish() {
                if (settled) { return; }
                settled = true;
                delete requests[nonce];
                resolve();
            }
            requests[nonce] = finish;
            // A request that is never answered -- a module error, a dropped
            // socket -- must not leave the download hanging forever. The
            // archive then goes out without images rather than not at all.
            setTimeout(finish, ROUNDTRIP_TIMEOUT_MS);
            Shiny.setInputValue(btn.id + '_images',
                { nonce: nonce, images: images }, { priority: 'event' });
        });
    }

    if (typeof Shiny !== 'undefined' && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler('vizmodules-source-images', function (msg) {
            var nonce = msg ? unbox(msg.nonce) : null;
            var finish = nonce ? requests[nonce] : null;
            if (finish) { finish(); }
        });
    }

    // The capture takes a moment on a heavy figure. Without this the button
    // looks broken, and a second click would start a second capture.
    function setBusy(btn, on) {
        if (on) {
            btn.setAttribute('data-viz-source-busy', '1');
            btn.setAttribute('aria-busy', 'true');
            btn.style.pointerEvents = 'none';
            btn.style.opacity = '0.65';
        } else {
            btn.removeAttribute('data-viz-source-busy');
            btn.removeAttribute('aria-busy');
            btn.style.pointerEvents = '';
            btn.style.opacity = '';
        }
    }

    function refire(btn) {
        btn.setAttribute('data-viz-source-armed', '1');
        // downloadButton() renders target="_blank", and a programmatic click on
        // a _blank link is exactly what a popup blocker looks for. Dropping it
        // makes this a same-tab navigation to a Content-Disposition:attachment
        // URL, which downloads without leaving the page. Restored immediately,
        // so the markup the user sees is unchanged.
        var target = btn.getAttribute('target');
        if (target) { btn.removeAttribute('target'); }
        btn.click();
        if (target) { btn.setAttribute('target', target); }
        setTimeout(function () { btn.removeAttribute('data-viz-source-armed'); },
            REARM_TIMEOUT_MS);
    }

    document.addEventListener('click', function (e) {
        var btn = e.target && e.target.closest
            ? e.target.closest('a.viz-source-download') : null;
        if (!btn) { return; }

        // Our own re-fired click: let the browser get on with the download.
        if (btn.getAttribute('data-viz-source-armed') === '1') {
            btn.removeAttribute('data-viz-source-armed');
            return;
        }
        if (btn.getAttribute('data-viz-source-busy') === '1') {
            e.preventDefault();
            return;
        }
        if (typeof Shiny === 'undefined' || !Shiny.setInputValue) { return; }

        var found = targets(btn);
        // Nothing to photograph: leave the button to behave exactly as it did
        // before this script existed.
        if (!found.length) { return; }

        e.preventDefault();
        setBusy(btn, true);

        function proceed() {
            setBusy(btn, false);
            refire(btn);
        }

        try {
            captureAll(found)
                .then(function (images) { return request(btn, images); })
                .catch(function () { /* fall through to the download regardless */ })
                .then(proceed);
        } catch (err) {
            // Whatever went wrong, the user still gets their archive.
            proceed();
        }
    });

    window.VizModules.sourceExport = {
        targets: targets,
        capture: captureAll,
        requests: requests
    };
})();
