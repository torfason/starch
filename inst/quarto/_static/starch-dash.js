/* starch-dash.js -- shared client-side helpers for the Quarto dashboard.
 *
 * Everything here is deliberately plain, classic-script JavaScript. Classic
 * scripts are not subject to the CORS restrictions that block ES modules from
 * file:// URLs, which is what lets these pages be opened by double-clicking
 * them. Do not convert this file to an ES module, and do not use fetch() or
 * dynamic import() anywhere in it: both are blocked on file://. Data arrives
 * by injecting further classic <script> tags instead.
 *
 * Depends on globals: d3, Plot (both loaded as UMD builds from lib/).
 */
(function (global) {
  "use strict";

  var S = {};

  /* ---- formatting ------------------------------------------------------ */

  var DASH = "\u2014";

  S.fmtDuration = function (s) {
    if (s === null || s === undefined || !isFinite(s)) return DASH;
    var h = Math.floor(s / 3600);
    var m = Math.floor((s % 3600) / 60);
    var ss = Math.round(s % 60);
    var pad = function (n) { return n < 10 ? "0" + n : String(n); };
    return h > 0 ? h + ":" + pad(m) + ":" + pad(ss) : m + ":" + pad(ss);
  };

  S.fmtPace = function (minPerKm) {
    if (!isFinite(minPerKm) || minPerKm <= 0) return DASH;
    var mins = Math.floor(minPerKm);
    var secs = Math.round((minPerKm - mins) * 60);
    if (secs === 60) { mins += 1; secs = 0; }
    return mins + ":" + (secs < 10 ? "0" + secs : secs);
  };

  S.fmtNum = function (v, digits) {
    if (v === null || v === undefined || !isFinite(v)) return DASH;
    return v.toFixed(digits === undefined ? 1 : digits);
  };

  S.fmtInt = function (v) {
    if (v === null || v === undefined || !isFinite(v)) return DASH;
    return Math.round(v).toLocaleString("en-US").replace(/,/g, "\u2009");
  };

  S.fmtDate = function (d) {
    if (!d) return DASH;
    var dt = d instanceof Date ? d : new Date(d);
    if (isNaN(dt)) return DASH;
    return dt.toISOString().slice(0, 16).replace("T", " ");
  };

  /* Foot-based activities are the ones where pace rather than speed is the
   * natural unit. Mirrors fmt_card_stats() on the R side. */
  var FOOT = { "Run": 1, "Trail Run": 1, "Walk": 1, "Hike": 1 };

  S.cardStats = function (a) {
    var dur = S.fmtDuration(a.elapsed_time_s);
    if (!isFinite(a.distance_km) || a.distance_km === 0) return dur;
    var out = S.fmtNum(a.distance_km, 1) + " km \u00b7 " + dur;
    if (FOOT[a.activity_type] && isFinite(a.elapsed_time_s)) {
      out += " \u00b7 " + S.fmtPace((a.elapsed_time_s / 60) / a.distance_km) + "/km";
    }
    return out;
  };

  /* ---- data access ----------------------------------------------------- */

  /* data/activities.js assigns window.STARCH_ACTIVITIES. Dates arrive as ISO
   * strings, since JSON has no date type; parse once, here, so that no caller
   * has to remember to. */
  var _acts = null;

  S.activities = function () {
    if (_acts === null) {
      _acts = (global.STARCH_ACTIVITIES || []).map(function (a) {
        var o = {};
        for (var k in a) if (Object.prototype.hasOwnProperty.call(a, k)) o[k] = a[k];
        o.date = a.activity_date ? new Date(a.activity_date) : null;
        return o;
      });
    }
    return _acts;
  };

  S.sports = function () {
    var seen = {};
    S.activities().forEach(function (a) {
      if (a.activity_type) seen[a.activity_type] = (seen[a.activity_type] || 0) + 1;
    });
    return Object.keys(seen).sort(function (x, y) { return seen[y] - seen[x]; });
  };

  /* ---- period bucketing ------------------------------------------------ */

  /* Bucket starts are computed in UTC throughout. Activity timestamps are
   * stored UTC by the reader, and using local time here would silently shift
   * activities across bucket boundaries depending on who opens the page. */
  S.periodStart = function (date, bucket) {
    var y = date.getUTCFullYear(), m = date.getUTCMonth(), d = date.getUTCDate();
    if (bucket === "year") return new Date(Date.UTC(y, 0, 1));
    if (bucket === "month") return new Date(Date.UTC(y, m, 1));
    // ISO weeks start Monday; getUTCDay() is 0 for Sunday.
    var dow = (date.getUTCDay() + 6) % 7;
    return new Date(Date.UTC(y, m, d - dow));
  };

  S.METRICS = {
    distance: { label: "Distance (km)", field: "distance_km", digits: 1 },
    time: { label: "Moving time (h)", field: "moving_time_s", digits: 1,
      scale: 1 / 3600 },
    elevation: { label: "Elevation gain (m)", field: "elevation_gain_m", digits: 0 },
    count: { label: "Activities", field: null, digits: 0 }
  };

  /* Aggregate to one row per (period, sport). Periods with no activity are
   * absent rather than zero, which is what Plot's rect marks want. */
  S.aggregate = function (rows, opts) {
    var metric = S.METRICS[opts.metric] || S.METRICS.distance;
    var keep = opts.sports && opts.sports.length ? opts.sports : null;
    var bins = {};

    rows.forEach(function (a) {
      if (!a.date || isNaN(a.date)) return;
      if (keep && keep.indexOf(a.activity_type) < 0) return;
      var start = S.periodStart(a.date, opts.bucket);
      var sport = a.activity_type || "(none)";
      var key = start.getTime() + "|" + sport;
      var val;
      if (metric.field === null) {
        val = 1;
      } else {
        val = a[metric.field];
        if (val === null || val === undefined || !isFinite(val)) return;
        if (metric.scale) val *= metric.scale;
      }
      if (!bins[key]) bins[key] = { start: start, sport: sport, value: 0 };
      bins[key].value += val;
    });

    return Object.keys(bins).map(function (k) { return bins[k]; })
      .sort(function (x, y) { return x.start - y.start; });
  };

  /* ---- charting -------------------------------------------------------- */

  /* Renders into el, replacing whatever was there. Plot returns a detached
   * SVG/figure element, so redrawing is a wholesale swap rather than an
   * update -- fine at these data volumes and much simpler to reason about. */
  S.drawTrends = function (el, opts) {
    var agg = S.aggregate(S.activities(), opts);
    el.innerHTML = "";
    if (agg.length === 0) {
      el.innerHTML = '<p class="starch-empty">No activities match the current selection.</p>';
      return;
    }
    var metric = S.METRICS[opts.metric] || S.METRICS.distance;
    var fig = Plot.plot({
      width: Math.max(640, el.clientWidth || 640),
      height: 380,
      marginLeft: 60,
      marginBottom: 40,
      x: { label: null, type: "utc" },
      y: { label: metric.label, grid: true },
      color: { legend: true },
      marks: [
        Plot.rectY(agg, {
          x: "start",
          interval: opts.bucket,
          y: "value",
          fill: "sport",
          tip: true
        }),
        Plot.ruleY([0])
      ]
    });
    el.appendChild(fig);
  };

  /* ---- detail data loading --------------------------------------------- */

  /* fetch() is unavailable on file://, so per-activity data is pulled in by
   * appending a classic <script> tag. Each data file assigns into the
   * STARCH_DETAIL registry, keyed by activity id, so load order and repeat
   * loads are both harmless. */
  global.STARCH_DETAIL = global.STARCH_DETAIL || {};

  S.loadDetail = function (id, done, fail) {
    if (global.STARCH_DETAIL[id]) { done(global.STARCH_DETAIL[id]); return; }
    var safe = String(id).replace(/[^A-Za-z0-9_-]/g, "");
    if (!safe) { fail("No activity id given."); return; }
    var s = document.createElement("script");
    s.src = "data/act_" + safe + ".js";
    s.onload = function () {
      var d = global.STARCH_DETAIL[id] || global.STARCH_DETAIL[safe];
      if (d) done(d); else fail("Data file loaded but held no activity " + id + ".");
    };
    s.onerror = function () { fail("No stream data for activity " + id + "."); };
    document.head.appendChild(s);
  };

  /* Read an activity id from either ?id= or #id=. The query string is the
   * canonical form; the hash is accepted too because it survives some
   * file-sharing paths that strip queries. */
  S.idFromUrl = function () {
    var q = new URLSearchParams(global.location.search).get("id");
    if (q) return q;
    var h = global.location.hash.replace(/^#/, "");
    if (h.indexOf("id=") === 0) return h.slice(3);
    return h || null;
  };

  /* ---- table ----------------------------------------------------------- */

  /* A deliberately small sortable/filterable/paged table. This replaces the
   * reactable widget of the Rmd dashboard: the point of the Quarto stack is
   * that pages are static shells and all data arrives from data/*.js, and an
   * R-generated widget would bake its own copy of the data into the page. */
  S.renderTable = function (el, rows, columns, opts) {
    opts = opts || {};
    var pageSize = opts.pageSize || 25;
    var state = { sort: opts.sort || null, desc: true, page: 0, query: "", sports: [] };

    var wrap = document.createElement("div");
    wrap.className = "starch-table";
    el.innerHTML = "";
    el.appendChild(wrap);

    function filtered() {
      var q = state.query.toLowerCase();
      var out = rows.filter(function (r) {
        if (state.sports.length && state.sports.indexOf(r.activity_type) < 0) return false;
        if (!q) return true;
        return columns.some(function (c) {
          var v = r[c.key];
          return v !== null && v !== undefined && String(v).toLowerCase().indexOf(q) >= 0;
        });
      });
      if (state.sort) {
        out = out.slice().sort(function (a, b) {
          var x = a[state.sort], y = b[state.sort];
          if (x === null || x === undefined) return 1;
          if (y === null || y === undefined) return -1;
          var c = (typeof x === "number" && typeof y === "number")
            ? x - y : String(x).localeCompare(String(y));
          return state.desc ? -c : c;
        });
      }
      return out;
    }

    function draw() {
      var data = filtered();
      var pages = Math.max(1, Math.ceil(data.length / pageSize));
      if (state.page >= pages) state.page = pages - 1;
      var slice = data.slice(state.page * pageSize, (state.page + 1) * pageSize);

      var head = "<tr>" + columns.map(function (c) {
        var mark = state.sort === c.key ? (state.desc ? " \u25be" : " \u25b4") : "";
        return '<th data-key="' + c.key + '">' + c.label + mark + "</th>";
      }).join("") + "</tr>";

      var body = slice.map(function (r) {
        return "<tr>" + columns.map(function (c) {
          return "<td>" + (c.cell ? c.cell(r[c.key], r) : (r[c.key] == null ? DASH : r[c.key])) + "</td>";
        }).join("") + "</tr>";
      }).join("");

      wrap.querySelector(".starch-thead").innerHTML = head;
      wrap.querySelector(".starch-tbody").innerHTML = body;
      wrap.querySelector(".starch-count").textContent =
        data.length + " of " + rows.length + " activities";
      wrap.querySelector(".starch-page").textContent =
        "Page " + (state.page + 1) + " of " + pages;
    }

    wrap.innerHTML =
      '<div class="starch-controls">' +
      '  <input class="starch-search" type="search" placeholder="Search...">' +
      '  <span class="starch-count"></span>' +
      '  <span class="starch-pager">' +
      '    <button class="starch-prev" type="button">Prev</button>' +
      '    <span class="starch-page"></span>' +
      '    <button class="starch-next" type="button">Next</button>' +
      "  </span>" +
      "</div>" +
      '<div class="starch-scroll"><table>' +
      '<thead class="starch-thead"></thead><tbody class="starch-tbody"></tbody>' +
      "</table></div>";

    wrap.querySelector(".starch-search").addEventListener("input", function (e) {
      state.query = e.target.value; state.page = 0; draw();
    });
    wrap.querySelector(".starch-prev").addEventListener("click", function () {
      if (state.page > 0) { state.page--; draw(); }
    });
    wrap.querySelector(".starch-next").addEventListener("click", function () {
      state.page++; draw();
    });
    wrap.querySelector(".starch-thead").addEventListener("click", function (e) {
      var th = e.target.closest("th");
      if (!th) return;
      var key = th.getAttribute("data-key");
      if (state.sort === key) state.desc = !state.desc;
      else { state.sort = key; state.desc = true; }
      draw();
    });

    draw();
    return { setSports: function (s) { state.sports = s; state.page = 0; draw(); } };
  };

  global.STARCH = S;
})(window);
