# Stylesheets leak into the host app

Every stylesheet this package ships is injected into the document of whatever app embeds a
module. There is no shadow DOM and no automatic scoping. Because each plot module renders a
`multiColorPicker`, attaching *any* module to a page pulls these stylesheets in — so one
careless selector restyles an app that merely wanted one plot.

Three separate leaks reached users this way (#355). All were the same mistake.

## The rule

Anchor every selector on a class this package invented: `.multi-color-picker`, `.mc-`,
`.mdi-`, `.vizmodules-`, `.viz-`, `.pb-`, `.a4-`.

```css
/* WRONG — every dropdown, sidebar and tab strip on the host page */
.selectize-dropdown .option { padding: 0 !important; }
.well .btn { padding: 4px 10px; }
.nav-tabs { flex-wrap: wrap; }

/* RIGHT */
.mc-palette-dropdown .option { padding: 0 !important; }
.pb-app .well .btn { padding: 4px 10px; }
.vizmodules-input-tabs > .nav-tabs { flex-wrap: wrap; }
```

These look scoped, which is the trap. `.well .btn` reads as "the buttons in my well", but
`.well` is what `shiny::sidebarPanel()` renders — the real comment in the source claimed it
was "scoped to the sidebar well" while restyling every sidebar on the page. **Judge by the
leftmost class: if you did not invent it, the rule is not scoped.**

Selectors belonging to someone else, seen in real leaks here: `.selectize-dropdown`,
`.option`, `.optgroup-header`, `.selectize-dropdown-content`, `.well`, `.nav-tabs`,
`.form-group`, `.control-label`, `.help-block`, `.btn`, `.progress`.

## Escaped elements need their own marker

A dropdown or popover parented to `<body>` (to dodge a clipping ancestor) cannot be reached
by `.my-widget .thing` — which is exactly why the picker's rules were left unscoped. Give
the escaped element a class instead:

```js
$(select).selectize({
  dropdownParent: "body",
  // Selectize replaces its default dropdownClass wholesale; restate its own.
  dropdownClass: "selectize-dropdown mc-palette-dropdown",
  ...
});
```

## Verify the selector matches something you own

`.selectize-dropdown .option` never matched the picker's own options: its custom
`render.option` emits `.mc-palette-option` and selectize does not add `.option` to it. The
rule's only effect was on other people's dropdowns — missing everything it was for, hitting
everything it wasn't. Inspect the element before trusting a rule.

## Layout

- **Never put layout in an inline `style=`.** It can only be overridden with `!important`,
  so a host app has to fight the widget instead of theming it. Layout goes in the
  stylesheet; per-instance values go through CSS custom properties
  (`style = "--viz-input-columns: 2;"`).
- **Do not assume the parent's padding.** Bootstrap's negative-margin row idiom
  (`margin-left: -15px` plus matching cell padding) overhangs in any container with less
  padding than that — it put a horizontal scrollbar in a `bslib::sidebar()`. Use `gap`.
- **`min-width: 0` on flex children**, or a long select or label refuses to shrink and
  pushes the container wider. Same overflow, different route.
- **Let rows wrap.** These controls land in sidebars of any width.

## Shipping one

`inst/src/<name>.css`, served by an `htmlDependency(src = "src", package = "VizModules")`,
attached to the markup that needs it with
`htmltools::attachDependencies(ui, dep, append = TRUE)` — not to the app as a whole, so the
styles travel through a runtime `insertUI()` (which the Figure Builder depends on).

Inline `tags$style()` blocks are held to the same standard: `.figure_builder_css()` and
`.data_filter_css()` go straight into the host's `<head>`.

## Checking it

`tests/testthat/test-ui_utils.R` parses every selector in every stylesheet — bundled and
inline — and fails on anything not anchored to a package prefix. Add your prefix there if
you introduce one.

That catches an unscoped selector but not a rule that does nothing. For the real effect,
render the widget beside a stock `selectInput()`, `sidebarPanel()` and `DT::datatable()`,
then disable only your sheet and re-measure:

```js
document.querySelectorAll('link[rel=stylesheet]').forEach(function (l) {
  if (l.href.indexOf('yourFile.css') !== -1) { l.disabled = true; }
});
```

A computed style changing on the host's own controls means you are leaking. Nothing
changing inside your widget means the rule never matched.
