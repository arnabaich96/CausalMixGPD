/* vignettes/theme-picker.js
   Full runtime theme editor with persistence
*/
(function(){
  const STORAGE_KEY = "pkgdown-runtime-theme-v1";

  const defaults = {
    bg: "#ffffff",
    fg: "#111827",
    muted: "#6b7280",
    border: "#e5e7eb",
    primary: "#2a6f97",
    link: "#2a6f97",
    codeBg: "#0b1220",
    codeFg: "#e5e7eb",

    baseFont: "system-ui",
    headingFont: "system-ui",
    monoFont: "ui-monospace",

    baseSize: 16,
    lineHeight: 1.6,
    radius: 14,
    space: 1.0
  };

  const googleFontMap = {
    "system-ui": null,
    "Inter": "Inter:wght@400;500;600;700",
    "Source Sans 3": "Source+Sans+3:wght@400;600;700",
    "IBM Plex Sans": "IBM+Plex+Sans:wght@400;600;700",
    "Merriweather": "Merriweather:wght@400;700",
    "Fira Code": "Fira+Code:wght@400;600",
    "JetBrains Mono": "JetBrains+Mono:wght@400;600"
  };

  function loadGoogleFont(fontName){
    const spec = googleFontMap[fontName];
    if(!spec) return;

    const id = "rt-font-" + fontName.replace(/\s+/g, "-").toLowerCase();
    if(document.getElementById(id)) return;

    const link = document.createElement("link");
    link.id = id;
    link.rel = "stylesheet";
    link.href = "https://fonts.googleapis.com/css2?family=" + spec + "&display=swap";
    document.head.appendChild(link);
  }

  function applyTheme(t){
    const r = document.documentElement.style;
    r.setProperty("--rt-bg", t.bg);
    r.setProperty("--rt-fg", t.fg);
    r.setProperty("--rt-muted", t.muted);
    r.setProperty("--rt-border", t.border);
    r.setProperty("--rt-primary", t.primary);
    r.setProperty("--rt-link", t.link);
    r.setProperty("--rt-code-bg", t.codeBg);
    r.setProperty("--rt-code-fg", t.codeFg);

    // Fonts (use fallbacks)
    const baseStack = (t.baseFont === "system-ui")
      ? 'system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, "Apple Color Emoji","Segoe UI Emoji"'
      : `"${t.baseFont}", system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial`;

    const headingStack = (t.headingFont === "system-ui")
      ? baseStack
      : `"${t.headingFont}", ${baseStack}`;

    const monoStack = (t.monoFont === "ui-monospace")
      ? 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", monospace'
      : `"${t.monoFont}", ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", monospace`;

    r.setProperty("--rt-font-base", baseStack);
    r.setProperty("--rt-font-heading", headingStack);
    r.setProperty("--rt-font-mono", monoStack);

    // Sizing
    r.setProperty("--rt-base-size", `${Number(t.baseSize)}px`);
    r.setProperty("--rt-line-height", String(t.lineHeight));
    r.setProperty("--rt-radius", `${Number(t.radius)}px`);
    r.setProperty("--rt-space", String(t.space));

    // Load fonts if needed
    loadGoogleFont(t.baseFont);
    loadGoogleFont(t.headingFont);
    loadGoogleFont(t.monoFont);
  }

  function getTheme(){
    try{
      const raw = localStorage.getItem(STORAGE_KEY);
      if(!raw) return {...defaults};
      const parsed = JSON.parse(raw);
      return {...defaults, ...parsed};
    }catch(e){
      return {...defaults};
    }
  }

  function setTheme(t){
    localStorage.setItem(STORAGE_KEY, JSON.stringify(t));
    applyTheme(t);
  }

  function resetTheme(){
    localStorage.removeItem(STORAGE_KEY);
    applyTheme({...defaults});
    syncUI({...defaults});
  }

  // --- UI Panel ---
  function el(tag, attrs={}, children=[]){
    const node = document.createElement(tag);
    Object.entries(attrs).forEach(([k,v]) => {
      if(k === "class") node.className = v;
      else if(k === "style") node.setAttribute("style", v);
      else node.setAttribute(k, v);
    });
    children.forEach(c => node.appendChild(c));
    return node;
  }

  function labelRow(text, input){
    const row = el("div", {class:"rt-row"});
    const lab = el("label", {class:"rt-label"});
    lab.textContent = text;
    row.appendChild(lab);
    row.appendChild(input);
    return row;
  }

  function makePanel(){
    const panel = el("div", {id:"rt-theme-panel", class:"rt-panel", "aria-hidden":"true"});

    panel.innerHTML = `
      <style>
        .rt-panel{
          position: fixed;
          right: 16px;
          bottom: 16px;
          width: 320px;
          max-height: 75vh;
          overflow: auto;
          z-index: 9999;
          background: rgba(255,255,255,.92);
          color: #111827;
          border: 1px solid rgba(0,0,0,.12);
          border-radius: 16px;
          box-shadow: 0 18px 60px rgba(0,0,0,.25);
          backdrop-filter: blur(10px);
          padding: 12px 12px 10px 12px;
          display: none;
        }
        [data-rt-open="true"] .rt-panel{ display:block; }
        .rt-title{ font-weight:700; margin: 4px 0 10px 0; }
        .rt-row{ display:flex; align-items:center; gap:10px; margin: 8px 0; }
        .rt-label{ flex: 1; font-size: 12px; opacity:.85; }
        .rt-row input[type="color"]{ width: 42px; height: 28px; padding:0; border: none; background: transparent; }
        .rt-row input[type="range"]{ width: 160px; }
        .rt-row select, .rt-row input[type="number"]{
          flex: 1;
          font-size: 12px;
          padding: 6px 8px;
          border-radius: 10px;
          border: 1px solid rgba(0,0,0,.15);
          background: white;
        }
        .rt-actions{ display:flex; gap:8px; margin-top: 10px; }
        .rt-actions button{
          flex:1;
          font-size: 12px;
          padding: 8px 10px;
          border-radius: 12px;
          border: 1px solid rgba(0,0,0,.15);
          background: white;
          cursor: pointer;
        }
        .rt-actions button:hover{ filter: brightness(.98); }
        .rt-fab{
          position: fixed;
          right: 16px;
          bottom: 16px;
          z-index: 9998;
          border-radius: 999px;
          padding: 10px 12px;
          border: 1px solid rgba(0,0,0,.12);
          background: rgba(255,255,255,.92);
          box-shadow: 0 18px 60px rgba(0,0,0,.20);
          backdrop-filter: blur(10px);
          cursor: pointer;
          font-size: 13px;
        }
      </style>
      <div class="rt-title">Theme Editor</div>
      <div id="rt-form"></div>
      <div class="rt-actions">
        <button id="rt-close" type="button">Close</button>
        <button id="rt-reset" type="button">Reset</button>
      </div>
    `;

    const fab = el("button", {id:"rt-theme-fab", class:"rt-fab", type:"button", title:"Customize theme"});
    fab.textContent = "Theme";

    document.body.appendChild(panel);
    document.body.appendChild(fab);

    return {panel, fab};
  }

  // Create inputs
  let ui = {};
  function buildForm(container, theme){
    function colorInput(key){
      const i = el("input", {type:"color", value: theme[key]});
      i.addEventListener("input", () => {
        theme[key] = i.value;
        setTheme(theme);
      });
      ui[key] = i;
      return i;
    }

    function selectInput(key, options){
      const s = el("select");
      options.forEach(opt => {
        const o = el("option", {value: opt});
        o.textContent = opt;
        s.appendChild(o);
      });
      s.value = theme[key];
      s.addEventListener("change", () => {
        theme[key] = s.value;
        setTheme(theme);
      });
      ui[key] = s;
      return s;
    }

    function rangeInput(key, min, max, step){
      const wrap = el("div", {style:"display:flex; align-items:center; gap:10px; flex:1;"});
      const r = el("input", {type:"range", min, max, step, value: theme[key]});
      const n = el("input", {type:"number", min, max, step, value: theme[key], style:"width:72px;"});
      function sync(v){
        theme[key] = Number(v);
        r.value = theme[key];
        n.value = theme[key];
        setTheme(theme);
      }
      r.addEventListener("input", () => sync(r.value));
      n.addEventListener("input", () => sync(n.value));
      wrap.appendChild(r);
      wrap.appendChild(n);
      ui[key] = {r, n};
      return wrap;
    }

    container.innerHTML = "";

    container.appendChild(labelRow("Background", colorInput("bg")));
    container.appendChild(labelRow("Text", colorInput("fg")));
    container.appendChild(labelRow("Muted text", colorInput("muted")));
    container.appendChild(labelRow("Border", colorInput("border")));
    container.appendChild(labelRow("Primary", colorInput("primary")));
    container.appendChild(labelRow("Link", colorInput("link")));
    container.appendChild(labelRow("Code background", colorInput("codeBg")));
    container.appendChild(labelRow("Code text", colorInput("codeFg")));

    const fontOptions = Object.keys(googleFontMap);
    container.appendChild(labelRow("Base font", selectInput("baseFont", fontOptions)));
    container.appendChild(labelRow("Heading font", selectInput("headingFont", fontOptions)));
    container.appendChild(labelRow("Mono font", selectInput("monoFont", fontOptions)));

    container.appendChild(labelRow("Base size (px)", rangeInput("baseSize", 12, 22, 1)));
    container.appendChild(labelRow("Line height", rangeInput("lineHeight", 1.2, 2.0, 0.05)));
    container.appendChild(labelRow("Radius (px)", rangeInput("radius", 0, 28, 1)));
    container.appendChild(labelRow("Spacing x", rangeInput("space", 0.8, 1.6, 0.05)));
  }

  function syncUI(theme){
    // For simple color & selects:
    ["bg","fg","muted","border","primary","link","codeBg","codeFg"].forEach(k => {
      if(ui[k]) ui[k].value = theme[k];
    });
    ["baseFont","headingFont","monoFont"].forEach(k => {
      if(ui[k]) ui[k].value = theme[k];
    });
    // Ranges:
    ["baseSize","lineHeight","radius","space"].forEach(k => {
      if(ui[k]) { ui[k].r.value = theme[k]; ui[k].n.value = theme[k]; }
    });
  }

  // Boot
  const theme = getTheme();
  applyTheme(theme);

  function init(){
    const {panel, fab} = makePanel();
    const form = panel.querySelector("#rt-form");
    buildForm(form, theme);

    function open(){
      document.documentElement.setAttribute("data-rt-open","true");
      panel.setAttribute("aria-hidden","false");
      syncUI(theme);
    }
    function close(){
      document.documentElement.removeAttribute("data-rt-open");
      panel.setAttribute("aria-hidden","true");
    }

    function toggle(){
      const openNow = document.documentElement.getAttribute("data-rt-open") === "true";
      if(openNow) close(); else open();
    }

    function attachNavbarButton(){
      const navLists = document.querySelectorAll(".navbar .navbar-nav");
      if(!navLists || navLists.length === 0) return null;
      const navList = navLists[navLists.length - 1];
      const li = el("li", {class:"nav-item"});
      const btn = el("button", {type:"button", class:"nav-link rt-navbar-btn", title:"Customize theme"});
      btn.textContent = "Theme";
      btn.addEventListener("click", toggle);
      li.appendChild(btn);
      navList.appendChild(li);
      return btn;
    }

    const navBtn = attachNavbarButton();
    if(navBtn && fab && fab.parentNode){
      fab.parentNode.removeChild(fab);
    }else{
      fab.addEventListener("click", toggle);
    }

    panel.querySelector("#rt-close").addEventListener("click", close);
    panel.querySelector("#rt-reset").addEventListener("click", resetTheme);

    // Close on ESC
    window.addEventListener("keydown", (e) => {
      if(e.key === "Escape") close();
    });
  }

  if(document.readyState === "loading"){
    document.addEventListener("DOMContentLoaded", init);
  }else{
    init();
  }
})();

